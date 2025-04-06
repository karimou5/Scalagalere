FROM eclipse-temurin:11-jdk

WORKDIR /app

# Installation des dépendances nécessaires
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    curl \
    wget \
    vim \
    nano \
    git \
    unzip \
    zip \
    && rm -rf /var/lib/apt/lists/*

# Installation de SBT
RUN echo "deb https://repo.scala-sbt.org/scalasbt/debian all main" | tee /etc/apt/sources.list.d/sbt.list && \
    echo "deb https://repo.scala-sbt.org/scalasbt/debian /" | tee /etc/apt/sources.list.d/sbt_old.list && \
    curl -sL "https://keyserver.ubuntu.com/pks/lookup?op=get&search=0x2EE0EA64E40A89B84B2DF73499E82A75642AC823" | apt-key add && \
    apt-get update && \
    apt-get install -y sbt=1.9.0 && \
    rm -rf /var/lib/apt/lists/*

# Copie du build.sbt pour pré-télécharger les dépendances
COPY build.sbt .
RUN mkdir -p src/main/scala src/test/scala
RUN touch src/main/scala/dummy.scala

# Pré-télécharger les dépendances SBT
RUN sbt update

# Le code sera monté comme volume
VOLUME ["/app"]

# Commande par défaut pour ouvrir un terminal
CMD ["bash"]