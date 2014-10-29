CREATE TABLE groupinfo (
    groupid int PRIMARY KEY NOT NULL auto_increment,
    name varchar(150),
    owner varchar(100) NOT NULL,
    created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP
) CHARACTER SET utf8;

CREATE TABLE groupuser (
    id int PRIMARY KEY NOT NULL auto_increment,
    groupid int NOT NULL,
    jid varchar(100) NOT NULL,
    joined_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP
) CHARACTER SET utf8;
CREATE INDEX group_jid_index ON groupuser (jid);
CREATE INDEX group_id_index ON groupuser (groupid);