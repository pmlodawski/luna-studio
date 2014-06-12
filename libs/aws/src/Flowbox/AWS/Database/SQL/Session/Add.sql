insert into sessions (user_name, instance_id, expires, policy) 
       values (?, ?, ?, ?)
       returning id