.PHONY: build shell compile test run clean

# Construire l'image Docker
build:
	docker-compose build

# Ouvrir un shell dans le container
shell:
	docker-compose run --rm scala bash

# Compiler le projet
compile:
	docker-compose run --rm scala sbt compile

# Exécuter les tests
test:
	docker-compose run --rm scala sbt test

# Lancer une classe spécifique (à utiliser comme `make run CLASS=HelloWorld`)
run:
	docker-compose run --rm scala sbt "runMain $(CLASS)"

# Nettoyer les conteneurs et les ressources
clean:
	docker-compose down
	docker-compose rm -f