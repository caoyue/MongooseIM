#!/bin/bash

source tools/travis-common-vars.sh

SQLDIR=${BASE}/apps/ejabberd/priv

MYSQL_PASSWORD=abcdefg

echo "Configuring mysql"
mysql -u root -e 'drop database IF EXISTS ejabberd'
mysql -u root -e 'create database IF NOT EXISTS ejabberd'
mysql -u root -e "grant all on ejabberd.* to 'ejabberd'@'localhost' identified by '${MYSQL_PASSWORD}'"
echo "Creating schema"
mysql -u ejabberd --password=${MYSQL_PASSWORD} ejabberd < ${SQLDIR}/mysql.sql
