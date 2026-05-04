# API Reference

## Health Check

```
GET /health
```

Returns `200 OK` with body `OK` when the service is running.

## Generate CSV

```
POST /api/v1/csv/generate
Content-Type: application/json
```

Submits a GraphQL query for CSV generation. The service processes the query asynchronously — it paginates through all results, flattens the JSON into CSV rows, uploads the file to S3, and emails a download link to the recipient.

### Request Body

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `shardId` | integer | yes | Tenant identifier (must be positive) |
| `recipient` | string | yes | Email address to receive the download link |
| `graphqlPaginationKey` | string | yes | Field used for cursor-based pagination (e.g. `"createdAt"`) |
| `graphqlQueryBody` | string | yes | GraphQL query string |
| `graphqlQueryVariables` | string | yes | JSON-encoded query variables |
| `columnConfig` | object | no | Inline column mapping (see [Column Presets](column-presets.md)) |
| `columnConfigName` | string | no | Name of a stored column preset (see [Column Presets](column-presets.md)) |

`columnConfig` and `columnConfigName` are mutually exclusive — specifying both is a validation error.

### Example Request

```json
{
  "shardId": 42,
  "recipient": "ops@example.com",
  "graphqlPaginationKey": "createdAt",
  "graphqlQueryBody": "query ($rowLimit: Int!, $paginationCondition: payment_requests_bool_exp!, $conditions: payment_requests_bool_exp!) { paymentRequests(limit: $rowLimit, where: {_and: [$paginationCondition, $conditions]}) { waitToken createdAt amount currency } }",
  "graphqlQueryVariables": "{\"conditions\":{\"createdAt\":{\"_gte\":\"2026-03-01T00:00:00Z\",\"_lt\":\"2026-03-15T00:00:00Z\"}}}"
}
```

### Success Response

```
HTTP/1.1 200 OK
Content-Type: application/json
```

```json
{
  "reportId": 12345
}
```

The `reportId` can be used to track progress in the `smart_csv.generated_csv` table.

### Error Response

```
HTTP/1.1 400 Bad Request
Content-Type: application/json
```

```json
{
  "error": "The query must define rowLimit to limit the number of rows."
}
```

### Validation Rules

**GraphQL query body** must:
- Be valid GraphQL syntax
- Contain exactly one root field
- Define a `$rowLimit` variable (used internally for pagination batch size)
- Define a `$paginationCondition` variable (injected by the service to paginate)

**Query variables** must:
- Be valid JSON
- Contain a `conditions` object
- Filter on the `graphqlPaginationKey` field in both directions using `_gte`/`_gt` and `_lt`/`_lte`
- The date range must not exceed 33 days

**Other fields:**
- `shardId` must be a positive integer
- `recipient` must not be empty

## How It Works

Once a request is accepted:

1. The service inserts a record into the database, which triggers a state machine and enqueues a background job.
2. The worker picks up the job, signs a JWT with the organization's claims, and begins paginating through the GraphQL endpoint.
3. Each page of results is converted into CSV rows using one column per selected field. Scalar fields are written directly, and nested object/array fields can be reduced to a single nested value via `columnConfig.dataPath`.
4. Rows are streamed to S3 as a multipart upload in ~5 MB chunks.
5. When complete, a presigned download URL is stored in the database and emailed to the recipient.

### JSON To CSV

Each selected GraphQL field produces at most one CSV column.

Given a GraphQL response row:

```json
{
  "payment_request_id": "wt_123",
  "customer": {
    "profile": {
      "email": "user@example.com",
      "name": "Ada"
    }
  },
  "attempts": [
    {
      "payment": {
        "cardType": "VISA"
      }
    },
    {
      "payment": {
        "cardType": "MASTERCARD"
      }
    }
  ]
}
```

With this column config:

```json
{
  "payment_request_id": { "header": "Payment Request ID" },
  "customer": { "header": "Customer Email", "dataPath": "profile.email" },
  "attempts": { "header": "Card Type", "dataPath": "payment.cardType" }
}
```

The CSV columns become:

| Payment Request ID | Customer Email | Card Type |
|--------------------|----------------|-----------|
| wt_123 | user@example.com | VISA,MASTERCARD |

Type conversions:
- Strings and numbers are used as-is
- Booleans become `"True"` or `"False"`
- Nulls become empty fields
- Arrays serialize every item and join the rendered values with commas
- Without `dataPath`, arrays and objects are unwrapped recursively while there is only one possible path forward (first array element, or an object with exactly one key)
- If the traversal reaches an object with multiple keys and there is no `dataPath`, no value is emitted for that column
