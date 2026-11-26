-- Deploy smart-csv:query_range_limit to pg
-- Requires: smart_csv/schemas
-- Requires: smart_csv/extensions

begin;

create table if not exists smart_csv.query_range_limit (
    root_name text_100 primary key,
    max_range_days integer not null check (max_range_days > 0),
    created_at timestamptz not null default now()
);

commit;
