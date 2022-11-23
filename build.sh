#!/bin/bash

DIR="`dirname $0`"

cd "$DIR"

sbt clean assembly

JAR=`find . -name '*.jar'`
echo "JAR built: $JAR"
echo "Run with: java -jar $JAR <source file name> <ram size>"

