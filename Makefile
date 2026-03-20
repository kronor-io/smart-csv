.PHONY: build test run setup-db clean docker-up docker-down

build:
	cabal build all

test:
	cabal test all

run: build
	bash _dev/smart-csv-runner.sh

docker-up:
	docker compose up -d

docker-down:
	docker compose down

setup-db:
	bash _dev/setup-db.sh

revert-db:
	cd database && sqitch revert db:pg://smart_csv:smart_csv@localhost:5432/smart_csv

clean:
	cabal clean
