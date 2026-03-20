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
3. Each page of results is flattened from nested JSON into flat key-value rows (e.g. `customer.email` becomes `paymentRequests_customer_email`).
4. Rows are streamed to S3 as a multipart upload in ~5 MB chunks.
5. When complete, a presigned download URL is stored in the database and emailed to the recipient.

### JSON Flattening

Nested GraphQL responses are flattened using underscore-separated paths, prefixed with the root query field name.

Given a GraphQL response row:

```json
{
  "waitToken": "wt_123",
  "customer": {
    "email": "user@example.com",
    "name": "Ada"
  }
}
```

With root field `paymentRequests`, the CSV columns become:

| paymentRequests_waitToken | paymentRequests_customer_email | paymentRequests_customer_name |
|---------------------------|-------------------------------|-------------------------------|
| wt_123 | user@example.com | Ada |

Type conversions:
- Strings and numbers are used as-is
- Booleans become `"True"` or `"False"`
- Nulls become empty fields
- Arrays use the first element only
- Nested objects are recursively flattened
