# Changelog: the `smart-csv` library

The library follows the [Haskell PVP](https://pvp.haskell.org/): a breaking change
bumps the first or second component. Runner releases (`v*` tags) are versioned
separately; see [docs/releases.md](../docs/releases.md).

## 0.2.0.0 (unreleased)

### Breaking changes and how to migrate

- **`Kronor.SmartCsv.Statements` is gone** (#16). The SQL statements moved into the
  runner; the library no longer depends on `hasql`. Hosts use their own statements.
- **Cells are keyed by column, not by header** (#17). Headers need not be unique, so
  a row's cells are keyed by `ColumnKey`: the selected field, and the entry's
  position for a field with `columns`.
  - `csvify` returns `Map ColumnKey Csv.Field` and `xlsxify` returns
    `Map ColumnKey XlsxCell`.
  - `csvRowBytes`, `xlsxRowBytes`, `decodeResponsePage`, `decodeResponseChunk`,
    their `With` variants and `decodeResponseRows` take the file's column keys in
    file order instead of a header vector (and an empty row):
    `Vector.fromList ((.key) <$> inferColumns config rootSelection)`. Write the
    header row from `(.header) <$> inferColumns config rootSelection`, or
    `inferHeaders`.
  - `DecodedResponsePage.lastRow` is keyed by `ColumnKey`.
- **`extractCursor` is removed** (#17). It read the cursor from a flattened row by
  header, which is wrong once a field prints several columns. Use
  `extractCursorValue` on the raw response row (`DecodedResponsePage.lastRawRow`);
  it takes the root selection and the pagination fields, not the column config.
- **`columnHeader`, `columnDataPath`, `columnDecimalPlaces` and `columnIsNumeric`
  are removed** (#17). They described one column per field. Use `outputColumns`,
  which gives every column a field prints.
- **`numericColumns` returns and `markNumericColumns` takes `Set ColumnKey`**
  instead of `Set Text` (#17). A field without `columns` is
  `ColumnKey field Nothing`.
- **`ColumnSettings` has a fifth field, `columns`** (#17). Code that builds it
  positionally adds `Nothing`, or starts from `defaultColumnSettings`.

### Added

- `columns` in a column config: one selected field prints several columns, each
  with its own `header`, `dataPath` and `decimalPlaces` (#17).
- `ColumnKey`, `OutputColumn`, `outputColumns`, `defaultColumnSettings` and
  `inferColumns` (#17).
- `excelMaxColumns` and `checkColumnCount`, to refuse an Excel export with more
  columns than a sheet holds (#17).
- Version bounds on every dependency. They allow both the runner's versions and
  Kronor's (morpheus-graphql-core 0.27 and 0.28, aeson 2.2 and 2.3, containers
  0.6 and 0.7), and CI builds and tests the library with each (#21).

## 0.1.0.0

The library as split out of Kronor's CSV export.
