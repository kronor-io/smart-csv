-- Extensions required by smart-csv
CREATE EXTENSION IF NOT EXISTS citext;
CREATE EXTENSION IF NOT EXISTS pgcrypto;
CREATE EXTENSION IF NOT EXISTS pgjwt;
CREATE EXTENSION IF NOT EXISTS semver;

-- JWT secret used by signJwtFromClaims (must match the key used by Hasura / the API)
ALTER DATABASE smart_csv SET "graphql.jwt_secret" = 'CluqFpAjf9eVcy1gSnrcZWOIKiGBoLl651D7TE0l4Dc=';
