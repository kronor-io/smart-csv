# Column Presets

By default, CSV columns use the selected GraphQL field names as headers.
Column presets let you rename columns to human-readable headers, control numeric
formatting, and extract a nested value from object or array results using a
dot-separated `dataPath`.

## Config Format

A column config is a JSON object where:

- **Keys** are selected GraphQL field names or aliases
- **Values** are objects with optional settings:
  - `header` — CSV header to display for that field
  - `decimalPlaces` — number of decimal places for numeric formatting; if omitted,
    numeric values are kept as-is. Note: this **truncates** toward zero rather than
    rounding, so `12.35` with `decimalPlaces: 1` becomes `12.3` not `12.4`.
  - `dataPath` — dot-separated path used to extract a nested value from an object.
    When the *top-level field* is an array every element is processed and the results
    are joined with `, `. When an intermediate step in the path encounters an array,
    only the **first element** is followed.

Fields not mentioned in the config are included with their raw field name as the
header.

If `dataPath` is omitted, Smart CSV still tries to resolve a scalar value by
following the only available route through nested data:

- When the field value is an **array**, every element is rendered and joined with
  `, ` (same as when `dataPath` is set)
- When the field value is an **object with exactly one key**, traversal follows
  that key automatically
- Traversal stops once a scalar is found
- If an object has multiple keys and no `dataPath` is provided, the column is
  left blank

### Example

Given a GraphQL query that returns `payment_request_id`, `placed_at`,
`customer { profile { email } }`, and `attempts { payment { cardType } }`:

```json
{
  "payment_request_id": {
    "header": "Order ID"
  },
  "placed_at": {
    "header": "Date"
  },
  "customer": {
    "header": "Customer Email",
    "dataPath": "profile.email"
  },
  "attempts": {
    "header": "Card Type",
    "dataPath": "payment.cardType"
  }
}
```

This produces a CSV with four columns — `Order ID`, `Date`, `Customer Email`,
and `Card Type`.

For example, if `customer` returns `{ "profile": { "email": "user@example.com" } }`,
the value can still be resolved without a `dataPath` because each nested object
has only one key. But if `customer` returns `{ "email": "user@example.com", "name": "Ada" }`,
you need `dataPath` to disambiguate which value should be written.

Likewise, if `attempts` returns an array like
`[{ "payment": { "cardType": "VISA" } }, { "payment": { "cardType": "MASTERCARD" } }]`,
the CSV value becomes `VISA,MASTERCARD` when using `dataPath: "payment.cardType"`.

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
    "payment_request_id": { "header": "Order ID" },
    "amount": { "header": "Amount", "decimalPlaces": 2 },
    "customer": { "header": "Customer Email", "dataPath": "profile.email" }
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
    "payment_request_id": {"header": "Order ID"},
    "placed_at": {"header": "Date"},
    "amount": {"header": "Amount", "decimalPlaces": 2},
    "currency": {"header": "Currency"},
    "reference": {"header": "Reference"},
    "payment_method": {"header": "Payment Method"},
    "customer": {"header": "Email", "dataPath": "profile.email"},
    "latest_status": {"header": "Status"}
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
  "payment_request_id": {"header": "Order ID"},
  "placed_at": {"header": "Created At"},
  "amount": {"header": "Total", "decimalPlaces": 2}
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
  are included with their raw selected field names as headers.
- If **`columnConfig`** is provided, it is used directly.
- If **`columnConfigName`** is provided, the config is loaded from the database.
- Specifying **both** is a validation error.

## Tips

- To figure out what config keys to use, first run a request without any column
  config. The resulting CSV headers show the raw selected field names.
- Use `dataPath` when the selected field returns an object or array and there
  is more than one possible nested value you might want in the CSV.
- The pagination key field (e.g. `createdAt`) is used internally for
  cursor-based pagination. If you suppress it via column config, pagination will
  break. Keep it in the output or at least don't map it to `null`.
