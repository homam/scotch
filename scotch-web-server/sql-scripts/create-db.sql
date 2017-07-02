CREATE TYPE async_task_status AS ENUM ('NotStarted', 'Started', 'Completed', 'Failed')
CREATE TYPE gateway_notification_type AS ENUM ('SubscriptionNotification', 'BillingNotification', 'UnsubscriptionNotification');
CREATE TYPE gateway_connection AS ENUM ('PayguruTurkey');
CREATE TYPE opt_in_method as ENUM ('RedirectToPaymentPage');
CREATE TYPE pixel_state as ENUM ('NotProcessed', 'Cancelled', 'Scrubbed', 'UrlGenerationFailed', 'FiredAndSucceed', 'FiredAndFailed', 'Delayed');



CREATE TABLE visits (
  visit_id bigserial CONSTRAINT firstkey PRIMARY KEY,
  creation_time timestamp with time zone not null default now(),
  campaign_id int not null,
  landing_page varchar(50) not null,
  ip inet,
  ip_country char(2),
  headers json,
  query_params json,
  raw_path varchar(2048) not null,
  raw_query_string varchar(4096),
  gateway_connection gateway_connection null,
  opt_in_method opt_in_method null
);

CREATE UNIQUE INDEX ON visits (visit_id DESC);
CREATE INDEX ON visits (creation_time DESC);
CREATE INDEX ON visits (campaign_id);
CREATE INDEX ON visits (landing_page);
CREATE INDEX ON visits (lower(ip_country));


create TABLE pixels (
  creation_time timestamp with time zone not null default now()
, last_updated timestamp with time zone
, subscriber_id bigint not null
, visit_id bigint
, pixel_state pixel_state not null
, pixel_url varchar(2048)
, query_params json
, pixel_value decimal
, pixel_result_status_code int
, pixel_resul_text varchar(2048)
);
CREATE UNIQUE INDEX ON pixels (subscriber_id DESC);
CREATE INDEX ON pixels (creation_time DESC);
CREATE INDEX ON pixels (pixel_state);

--- UPDATE pg_enum SET enumlabel = 'PayguruTurkey' WHERE enumtypid = 'gateway_connection'::regtype AND enumlabel = 'PayGuruStandard';


-- example
-- insert into visits (campaign_id, landing_page_id, ip, ip_country, headers, query_params) VALUES (1, 1, '127.0.0.1'::inet, '--', null, null);

---
create TABLE gateway_notifications (
  gateway_notification_id bigserial CONSTRAINT gateway_notification_id PRIMARY KEY
, creation_time timestamp with time zone not null default now()
, all_params json not null
, raw_path varchar(2048) not null
, raw_query_string varchar(4096)
, notification_type gateway_notification_type not null
, gateway_connection gateway_connection not null
, task_status async_task_status not null
, task_result varchar(4096) null
, task_last_updated_time timestamp with time zone null
);
CREATE INDEX ON gateway_notifications (gateway_notification_id DESC);
CREATE INDEX ON gateway_notifications (notification_type DESC);
CREATE INDEX ON gateway_notifications (gateway_connection DESC);


---

CREATE TABLE integration_payguru_billings (
  integration_payguru_billing_id bigserial CONSTRAINT integration_payguru_billing_id PRIMARY KEY,
  creation_time timestamp with time zone not null default now(),
  status smallint,
  operator smallint,
  transactionId varchar(50),
  subsId varchar(50),
  service varchar(50),
  errorCode  varchar(50),
  errorDescription  varchar(1000),
  billingType varchar(50),
  query_params json,
  full_url varchar(2000)
);
CREATE UNIQUE INDEX ON integration_payguru_billings (integration_payguru_billing_id DESC);
CREATE INDEX ON integration_payguru_billings (creation_time DESC);

CREATE TABLE integration_payguru_sales (
  integration_payguru_sale_id bigserial CONSTRAINT integration_payguru_sale_id PRIMARY KEY,
  creation_time timestamp with time zone not null default now(),
  transactionId varchar(50),
  subsId varchar(50),
  service varchar(50),
  status smallint,
  operator smallint, -- oId in PG-API-005-General Payment API Turkey V1.1.pdf page 25
  processing_status async_task_status,
  processing_updated_time timestamp with time zone not null default now(),
)
