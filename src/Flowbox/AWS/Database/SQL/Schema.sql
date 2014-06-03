drop schema public cascade;
create schema public;

create table if not exists users (
    -- key         serial       not null       primary key,
    -- real_name   varchar(256) not null,
    -- email       varchar(256) not null       unique,
    name        varchar( 80) not null  primary key,

    salt        varchar( 16) not null  unique,
    password    varchar( 64) not null,   -- SHA3-256 (a.k.a Keecak) hash length
    credit      bigint       null      constraint positive_credit check (credit >= 0)
);

-- create type instance_status as enum ('Running', 'Stopped');

create table if not exists instances (
    -- key         serial          not null       primary key,
    id          varchar( 10)  not null  primary key,
    started_at  timestamptz   not null  default current_timestamp,
    status      varchar( 10)  not null  -- instance_status
);

-- create type policy_type as enum ('Autocharge', 'Halt');

create table if not exists sessions (
    id          serial        not null  primary key,
    user_name   varchar( 80)  not null  references users     on delete cascade,
    instance_id varchar( 10)  not null  references instances on delete cascade,
    expires     timestamptz   not null  constraint expires_in_the_future check (expires > current_timestamp),
    policy      varchar( 10)  not null  default 'Autocharge' -- policy_type
);
