# Column Presets

By default, CSV columns use the raw flattened field paths from the GraphQL
response (e.g. `paymentRequests_customer_email`). Column presets let you rename
columns to human-readable headers or suppress fields you don't want in the
output.

## Config Format

A column config is a JSON object where:

- **Keys** are flattened field paths (the underscore-separated paths produced by
  JSON flattening)
- **Values** are either:
  - A **string** — renames the column to that display name
  - **`null`** — suppresses the column entirely (excluded from the CSV)

Fields not mentioned in the config are included with their raw path as the
header.

### Example

Given a GraphQL query that returns `waitToken`, `createdAt`, `amount`, and
`customer.email` under the root field `paymentRequests`:

```json
{
  "paymentRequests_waitToken": "Order ID",
  "paymentRequests_createdAt": "Date",
  "paymentRequests_amount": "Amount",
  "paymentRequests_customer_email": null
}
```

This produces a CSV with three columns — `Order ID`, `Date`, `Amount` — and
`customer_email` is excluded.

## Inline Config

Pass the config directly in the request body using the `columnConfig` field:

```json
{
  "shardId": 42,
  "recipient": "ops@example.com",
  "graphqlPaginationKey": "createdAt",
  "graphqlQueryBody": "...",
  "graphqlQueryVariables": "...",
  "columnConfig": {
    "paymentRequests_waitToken": "Order ID",
    "paymentRequests_amount": "Amount",
    "paymentRequests_createdAt": "Date"
  }
}
```

This is useful for one-off exports where the column mapping is specific to a
single request.

## Named Presets

For column mappings that are reused across many requests, store them as named
presets in the database and reference them by name.

### Creating a Preset

Insert into the `smart_csv.column_config` table:

```sql
INSERT INTO smart_csv.column_config (name, config)
VALUES (
  'payment_requests_report',
  '{
    "paymentRequests_waitToken": "Order ID",
    "paymentRequests_createdAt": "Date",
    "paymentRequests_amount": "Amount",
    "paymentRequests_currency": "Currency",
    "paymentRequests_reference": "Reference",
    "paymentRequests_paymentProvider": "Payment Method",
    "paymentRequests_customer_name": "Customer",
    "paymentRequests_customer_email": "Email",
    "paymentRequests_currentStatus": "Status"
  }'::jsonb
);
```

### Using a Preset

Reference it by name in the request:

```json
{
  "shardId": 42,
  "recipient": "ops@example.com",
  "graphqlPaginationKey": "createdAt",
  "graphqlQueryBody": "...",
  "graphqlQueryVariables": "...",
  "columnConfigName": "payment_requests_report"
}
```

### Updating a Preset

```sql
UPDATE smart_csv.column_config
SET config = '{
  "paymentRequests_waitToken": "Order ID",
  "paymentRequests_createdAt": "Created At",
  "paymentRequests_amount": "Total"
}'::jsonb
WHERE name = 'payment_requests_report';
```

### Listing Presets

```sql
SELECT name, config, created_at
FROM smart_csv.column_config
ORDER BY name;
```

## Precedence

- If **neither** `columnConfig` nor `columnConfigName` is provided, all fields
  are included with their raw flattened paths as headers.
- If **`columnConfig`** is provided, it is used directly.
- If **`columnConfigName`** is provided, the config is loaded from the database.
- Specifying **both** is a validation error.

## Tips

- To figure out what the flattened field paths look like, first run a request
  without any column config. The resulting CSV headers show the raw paths — use
  those as keys in your config.
- The pagination key field (e.g. `createdAt`) is used internally for
  cursor-based pagination. If you suppress it via column config, pagination will
  break. Keep it in the output or at least don't map it to `null`.
