--
-- ejabberd, Copyright (C) 2002-2011   ProcessOne
--
-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License as
-- published by the Free Software Foundation; either version 2 of the
-- License, or (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
-- 02111-1307 USA
--

-- Needs MySQL (at least 4.0.x) with innodb back-end

CREATE TABLE users (
    username varchar(250) PRIMARY KEY,
    password text NOT NULL,
    pass_details text,
    email varchar(250),
    cellphone varchar(50),
    token varchar(250),
    active boolean DEFAULT true,
    created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP
) CHARACTER SET utf8;

CREATE INDEX i_users_email ON users(email);
CREATE INDEX i_users_cellphone ON users(cellphone);

CREATE TABLE last (
    username varchar(250) PRIMARY KEY,
    seconds int NOT NULL,
    state text NOT NULl
) CHARACTER SET utf8;

CREATE INDEX i_last_seconds ON last(seconds);


CREATE TABLE rosterusers (
    username varchar(250) NOT NULL,
    jid varchar(250) NOT NULL,
    nick text NOT NULL,
    subscription character(1) NOT NULL,
    ask character(1) NOT NULL,
    askmessage text NOT NULL,
    server character(1) NOT NULL,
    subscribe text NOT NULL,
    type text,
    private boolean NOT NULL DEFAULT false,
    created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP
) CHARACTER SET utf8;

CREATE UNIQUE INDEX i_rosteru_user_jid ON rosterusers(username(75), jid(75));
CREATE INDEX i_rosteru_username ON rosterusers(username);
CREATE INDEX i_rosteru_jid ON rosterusers(jid);

CREATE TABLE rostergroups (
    username varchar(250) NOT NULL,
    jid varchar(250) NOT NULL,
    grp text NOT NULL
) CHARACTER SET utf8;

CREATE INDEX pk_rosterg_user_jid ON rostergroups(username(75), jid(75));


CREATE TABLE spool (
    username varchar(250) NOT NULL,
    xml text NOT NULL,
    seq BIGINT UNSIGNED NOT NULL AUTO_INCREMENT UNIQUE,
    created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP
) CHARACTER SET utf8;

CREATE INDEX i_despool USING BTREE ON spool(username);


CREATE TABLE vcard (
    username varchar(150),
    server varchar(150),
    vcard mediumtext NOT NULL,
    created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
    PRIMARY KEY (username, server)
) CHARACTER SET utf8;


CREATE TABLE vcard_search (
    username varchar(150) NOT NULL,
    lusername varchar(100),
    server varchar(150),
    fn text NOT NULL,
    lfn varchar(250) NOT NULL,
    family text NOT NULL,
    lfamily varchar(250) NOT NULL,
    given text NOT NULL,
    lgiven varchar(250) NOT NULL,
    middle text NOT NULL,
    lmiddle varchar(250) NOT NULL,
    nickname text NOT NULL,
    lnickname varchar(250) NOT NULL,
    bday text NOT NULL,
    lbday varchar(250) NOT NULL,
    ctry text NOT NULL,
    lctry varchar(250) NOT NULL,
    locality text NOT NULL,
    llocality varchar(250) NOT NULL,
    email text NOT NULL,
    lemail varchar(250) NOT NULL,
    tel text NOT NULL,
    ltel varchar(250) NOT NULL,
    orgname text NOT NULL,
    lorgname varchar(250) NOT NULL,
    orgunit text NOT NULL,
    lorgunit varchar(250) NOT NULL,
    PRIMARY KEY (lusername, server)
) CHARACTER SET utf8;

CREATE INDEX i_vcard_search_server    ON vcard_search(server);
CREATE INDEX i_vcard_search_lfn       ON vcard_search(lfn);
CREATE INDEX i_vcard_search_lfamily   ON vcard_search(lfamily);
CREATE INDEX i_vcard_search_lgiven    ON vcard_search(lgiven);
CREATE INDEX i_vcard_search_lmiddle   ON vcard_search(lmiddle);
CREATE INDEX i_vcard_search_lnickname ON vcard_search(lnickname);
CREATE INDEX i_vcard_search_lbday     ON vcard_search(lbday);
CREATE INDEX i_vcard_search_lctry     ON vcard_search(lctry);
CREATE INDEX i_vcard_search_llocality ON vcard_search(llocality);
CREATE INDEX i_vcard_search_lemail    ON vcard_search(lemail);
CREATE INDEX i_vcard_search_ltel      ON vcard_search(ltel);
CREATE INDEX i_vcard_search_lorgname  ON vcard_search(lorgname);
CREATE INDEX i_vcard_search_lorgunit  ON vcard_search(lorgunit);

CREATE TABLE privacy_default_list (
    username varchar(250) PRIMARY KEY,
    name varchar(250) NOT NULL
) CHARACTER SET utf8;

CREATE TABLE privacy_list (
    username varchar(250) NOT NULL,
    name varchar(250) NOT NULL,
    id BIGINT UNSIGNED NOT NULL AUTO_INCREMENT UNIQUE,
    created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP
) CHARACTER SET utf8;

CREATE INDEX i_privacy_list_username  USING BTREE ON privacy_list(username);
CREATE UNIQUE INDEX i_privacy_list_username_name USING BTREE ON privacy_list (username(75), name(75));

CREATE TABLE privacy_list_data (
    id bigint,
    t character(1) NOT NULL,
    value text NOT NULL,
    action character(1) NOT NULL,
    ord bigint NOT NULL,
    match_all boolean NOT NULL,
    match_iq boolean NOT NULL,
    match_message boolean NOT NULL,
    match_presence_in boolean NOT NULL,
    match_presence_out boolean NOT NULL
) CHARACTER SET utf8;

CREATE TABLE private_storage (
    username varchar(250) NOT NULL,
    namespace varchar(250) NOT NULL,
    data text NOT NULL,
    created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP
) CHARACTER SET utf8;

CREATE INDEX i_private_storage_username USING BTREE ON private_storage(username);
CREATE UNIQUE INDEX i_private_storage_username_namespace USING BTREE ON private_storage(username(75), namespace(75));

-- Not tested in mysql
CREATE TABLE roster_version (
    username varchar(250) PRIMARY KEY,
    version text NOT NULL
) CHARACTER SET utf8;

-- To update from 1.x:
-- ALTER TABLE rosterusers ADD COLUMN askmessage text AFTER ask;
-- UPDATE rosterusers SET askmessage = '';
-- ALTER TABLE rosterusers ALTER COLUMN askmessage SET NOT NULL;

CREATE TABLE pubsub_node (
  host text,
  node text,
  parent text,
  type text,
  nodeid bigint auto_increment primary key
) CHARACTER SET utf8;
CREATE INDEX i_pubsub_node_parent ON pubsub_node(parent(120));
CREATE UNIQUE INDEX i_pubsub_node_tuple ON pubsub_node(host(20), node(120));

CREATE TABLE pubsub_node_option (
  nodeid bigint,
  name text,
  val text
) CHARACTER SET utf8;
CREATE INDEX i_pubsub_node_option_nodeid ON pubsub_node_option(nodeid);
ALTER TABLE `pubsub_node_option` ADD FOREIGN KEY (`nodeid`) REFERENCES `pubsub_node` (`nodeid`) ON DELETE CASCADE;

CREATE TABLE pubsub_node_owner (
  nodeid bigint,
  owner text
) CHARACTER SET utf8;
CREATE INDEX i_pubsub_node_owner_nodeid ON pubsub_node_owner(nodeid);
ALTER TABLE `pubsub_node_owner` ADD FOREIGN KEY (`nodeid`) REFERENCES `pubsub_node` (`nodeid`) ON DELETE CASCADE;

CREATE TABLE pubsub_state (
  nodeid bigint,
  jid text,
  affiliation character(1),
  subscriptions text,
  stateid bigint auto_increment primary key
) CHARACTER SET utf8;
CREATE INDEX i_pubsub_state_jid ON pubsub_state(jid(60));
CREATE UNIQUE INDEX i_pubsub_state_tuple ON pubsub_state(nodeid, jid(60));
ALTER TABLE `pubsub_state` ADD FOREIGN KEY (`nodeid`) REFERENCES `pubsub_node` (`nodeid`) ON DELETE CASCADE;

CREATE TABLE pubsub_item (
  nodeid bigint,
  itemid text,
  publisher text,
  creation text,
  modification text,
  payload text
) CHARACTER SET utf8;
CREATE INDEX i_pubsub_item_itemid ON pubsub_item(itemid(36));
CREATE UNIQUE INDEX i_pubsub_item_tuple ON pubsub_item(nodeid, itemid(36));
ALTER TABLE `pubsub_item` ADD FOREIGN KEY (`nodeid`) REFERENCES `pubsub_node` (`nodeid`) ON DELETE CASCADE;

CREATE TABLE pubsub_subscription_opt (
  subid text,
  opt_name varchar(32),
  opt_value text
);
CREATE UNIQUE INDEX i_pubsub_subscription_opt ON pubsub_subscription_opt(subid(32), opt_name(32));

CREATE TABLE mam_message(
  -- Message UID (64 bits)
  -- A server-assigned UID that MUST be unique within the archive.
  id BIGINT UNSIGNED NOT NULL,
  user_id INT UNSIGNED NOT NULL,
  -- FromJID used to form a message without looking into stanza.
  -- This value will be send to the client "as is".
  from_jid varchar(250) CHARACTER SET binary NOT NULL,
  -- The remote JID that the stanza is to (for an outgoing message) or from (for an incoming message).
  -- This field is for sorting and filtering.
  remote_bare_jid varchar(250) CHARACTER SET binary NOT NULL,
  remote_resource varchar(250) CHARACTER SET binary NOT NULL,
  -- I - incoming, remote_jid is a value from From.
  -- O - outgoing, remote_jid is a value from To.
  -- Has no meaning for MUC-rooms.
  direction ENUM('I','O') NOT NULL,
  -- Term-encoded message packet
  message blob NOT NULL,
  PRIMARY KEY (user_id, id),
  INDEX i_mam_message_rem USING BTREE (user_id, remote_bare_jid, id),
  INDEX i_mam_message_uid USING BTREE (user_id, id)
)  ENGINE=InnoDB
   PARTITION BY HASH(user_id)
   PARTITIONS 32;
-- Partition is selected based on MOD(user_id, 32)
-- See for more information
-- http://dev.mysql.com/doc/refman/5.1/en/partitioning-hash.html

CREATE TABLE mam_config(
  user_id INT UNSIGNED NOT NULL,
  -- If empty, than it is a default behaviour.
  remote_jid varchar(250) CHARACTER SET binary NOT NULL,
  -- A - always archive;
  -- N - never archive;
  -- R - roster (only for remote_jid == "")
  behaviour ENUM('A', 'N', 'R') NOT NULL
);
CREATE INDEX i_mam_config USING HASH ON mam_config(user_id, remote_jid);

CREATE TABLE mam_user(
  id INT UNSIGNED NOT NULL AUTO_INCREMENT,
  user_name varchar(250) CHARACTER SET binary NOT NULL,
  PRIMARY KEY(id) USING HASH,
  CONSTRAINT uc_mam_user_name UNIQUE (user_name)
);
CREATE INDEX i_mam_user_name USING HASH ON mam_user(user_name);

CREATE TABLE mam_server_user(
  id INT UNSIGNED NOT NULL AUTO_INCREMENT,
  server    varchar(250) CHARACTER SET binary NOT NULL,
  user_name varchar(250) CHARACTER SET binary NOT NULL,
  PRIMARY KEY(id) USING HASH,
  CONSTRAINT uc_mam_server_user_name UNIQUE (server, user_name)
);
CREATE INDEX i_mam_server_user_name USING HASH ON mam_server_user(server, user_name);


CREATE TABLE mam_muc_message(
  -- Message UID
  -- A server-assigned UID that MUST be unique within the archive.
  id BIGINT UNSIGNED NOT NULL PRIMARY KEY,
  room_id INT UNSIGNED NOT NULL,
  -- A nick of the message's originator
  nick_name varchar(250) NOT NULL,
  -- Term-encoded message packet
  message blob NOT NULL
);
CREATE INDEX i_mam_muc_message_room_name_added_at USING BTREE ON mam_muc_message(room_id, id);


CREATE TABLE offline_message(
  id BIGINT UNSIGNED        NOT NULL AUTO_INCREMENT PRIMARY KEY,
  timestamp BIGINT UNSIGNED NOT NULL,
  expire    BIGINT UNSIGNED,
  server    varchar(250)    NOT NULL,
  username  varchar(250)    NOT NULL,
  from_jid  varchar(250)    NOT NULL,
  packet    blob            NOT NULL
);
CREATE INDEX i_offline_message USING BTREE ON offline_message(server, username, id);


-- aft mod_groupchat tables begin
CREATE TABLE groupinfo (
    groupid int PRIMARY KEY NOT NULL auto_increment,
    name varchar(250) CHARACTER SET binary,
    owner varchar(250) CHARACTER SET binary NOT NULL,
    created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP
) CHARACTER SET utf8;

CREATE TABLE groupuser (
    id int PRIMARY KEY NOT NULL auto_increment,
    groupid int NOT NULL,
    jid varchar(250) CHARACTER SET binary NOT NULL,
    nickname varchar(250) CHARACTER SET binary,
    private boolean NOT NULL DEFAULT false,
    joined_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP
) CHARACTER SET utf8;
CREATE INDEX group_jid_index ON groupuser (jid);
CREATE INDEX group_id_index ON groupuser (groupid);
-- aft mod_groupchat tables end

CREATE TABLE privatemode (
    jid varchar(250) CHARACTER SET binary PRIMARY KEY NOT NULL,
    password varchar(250) CHARACTER SET binary NOT NULL
) CHARACTER SET utf8;

-- push service begin
CREATE TABLE push_service (
    id int PRIMARY KEY NOT NULL auto_increment,
    jid varchar(250) CHARACTER SET binary NOT NULL,
    token varchar(250) CHARACTER SET binary NOT NULL,
    push_type tinyint unsigned NOT NULL, -- push_type: 1. iOS, 2. Android, 3. Other
    last_login timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP
) CHARACTER SET utf8;
CREATE INDEX push_jid_index ON push_service (jid);
CREATE UNIQUE INDEX push_token_index ON push_service (token);
-- push service end