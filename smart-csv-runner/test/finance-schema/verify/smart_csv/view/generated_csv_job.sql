-- Verify finance:smart_csv/view/generated_csv_job on pg

select
    id,
    shard_id,
    user_id,
    query,
    recipient,
    pagination_key,
    number_of_rows,
    created_at,
    last_updated_at,
    last_pagination_time,
    expires_at,
    start_date,
    end_date,
    status,
    link,
    err_message
from smart_csv.generated_csv_job
where false;
