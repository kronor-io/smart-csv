-- Revert finance:smart_csv/view/generated_csv_job from pg

begin;

drop view if exists smart_csv.generated_csv_job;

commit;
