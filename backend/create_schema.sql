DROP TABLE IF EXISTS votes;
DROP TABLE IF EXISTS subscriptions;
DROP TABLE IF EXISTS forum_posts;
DROP TABLE IF EXISTS user_roles;
DROP TYPE  IF EXISTS role_enum;
DROP TABLE IF EXISTS forums;
DROP TABLE IF EXISTS users;

CREATE TABLE users (
  id BIGSERIAL PRIMARY KEY,
  user_name TEXT NOT NULL,
  password TEXT NOT NULL,
  created_at TIMESTAMP NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMP,
  deleted_at TIMESTAMP
);
CREATE UNIQUE INDEX user_name_idx ON users(user_name);

CREATE TABLE forums (
  id BIGSERIAL PRIMARY KEY,
  creator BIGINT NOT NULL REFERENCES users(id),
  name TEXT NOT NULL,
  description TEXT,
  created_at TIMESTAMP NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMP,
  deleted_at TIMESTAMP
);
CREATE UNIQUE INDEX forum_name_idx ON forums(name);

CREATE TYPE role_enum AS ENUM('Administrator', 'Poster', 'Reader');
CREATE TABLE user_roles (
  user_id BIGINT NOT NULL REFERENCES users(id),
  forum_id BIGINT NOT NULL REFERENCES forums(id),
  role_type role_enum NOT NULL,
  CONSTRAINT user_forum_role_idx PRIMARY KEY (user_id, forum_id)
);
CREATE INDEX user_role_role_idx ON user_roles(role_type);

CREATE TABLE forum_posts (
  id BIGSERIAL PRIMARY KEY,
  forum_id BIGINT NOT NULL REFERENCES forums(id),
  author_id BIGINT NOT NULL REFERENCE users(id),
  content TEXT NOT NULL,
  created_at TIMESTAMP NOT NULL DEFAULT NOW(),
  deleted_at TIMESTAMP,
  modified_from BIGINT REFERENCES forum_posts(id)
);

CREATE TABLE subscriptions (
  user_id BIGINT NOT NULL REFERENCES users(id),
  forum_id BIGINT NOT NULL REFERENCES forums(id),
  created_at TIMESTAMP NOT NULL DEFAULT NOW(),
  CONSTRAINT user_forum_sub_idx PRIMARY KEY (user_id, forum_id)
);

CREATE TABLE votes (
  id BIGSERIAL PRIMARY KEY,
  user_id BIGINT NOT NULL REFERENCES users(id),
  post_id BIGINT NOT NULL REFERENCES forum_posts(id),
  value SMALLINT NOT NULL,
  created_at TIMESTAMP NOT NULL DEFAULT NOW(),
  deleted_at TIMESTAMP 
);
CREATE UNIQUE INDEX user_post_vote_idx ON votes(user_id, post_id, deleted_at);
