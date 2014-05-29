drop schema public cascade;
create schema public;

create table if not exists users (
    key         serial       not null       primary key,
    -- real_name   varchar(256) not null,
    -- email       varchar(256) not null       unique,
    name        varchar( 80) not null       unique,

    salt        varchar( 16) not null       unique,
    password    varchar( 64) not null,   -- SHA3-256 (a.k.a Keecak) hash length
    credit      bigint       null           constraint positive_credit check (credit >= 0)
);

create type instance_status as enum ('started', 'halted');

create table if not exists instances (
    key         serial          not null       primary key,
    id          varchar( 10)    not null       unique,
    ip_addr     inet            not null       unique,
    started_at  timestamp       not null       default current_timestamp,
    status      instance_status not null
);

create type policy_type as enum ('autocharge', 'halt');

create table if not exists sessions (
    key          serial       not null       primary key,
    user_key     integer      not null       references users on delete cascade,
    instance_key integer      not null       references instances on delete cascade,
    expires      timestamp    not null       constraint expires_in_the_future check (expires > current_timestamp),
    policy       policy_type  not null       default 'autocharge'
);
