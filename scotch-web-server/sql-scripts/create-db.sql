CREATE TABLE visits (
  visit_id bigserial CONSTRAINT firstkey PRIMARY KEY,
  creation_time timestamp with time zone not null default now(),
  campaign_id int not null,
  landing_page_id int not null,
  ip inet,
  ip_country char(2),
  headers json,
  query_params json
);

CREATE UNIQUE INDEX ON visits (visit_id DESC);
CREATE INDEX ON visits (creation_time DESC);
CREATE INDEX ON visits (campaign_id);
CREATE INDEX ON visits (landing_page_id);
CREATE INDEX ON visits (lower(ip_country));

-- example
-- insert into visits (campaign_id, landing_page_id, ip, ip_country, headers, query_params) VALUES (1, 1, '127.0.0.1'::inet, '--', null, null);


CREATE TABLE integration_payguru_billings (
  integration_payguru_billing_id bigserial CONSTRAINT integration_payguru_billing_id PRIMARY KEY,
  creation_time timestamp with time zone not null default now(),
  status smallint,
  operator smallint,
  transactionId  varchar(50),
  subsId  varchar(50),
  service  varchar(50),
  errorCode  varchar(50),
  errorDescription  varchar(1000),
  billingType varchar(50),
  query_params json,
  full_url varchar(2000)
);
CREATE UNIQUE INDEX ON integration_payguru_billings (integration_payguru_billing_id DESC);
CREATE INDEX ON integration_payguru_billings (creation_time DESC);
