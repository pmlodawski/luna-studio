select * from instances
         where ? >= (select count (*) from sessions
                                      where sessions.instance_id = instances.id)
           and 0 = (select count (*) from sessions
                                     where sessions.instance_id = instances.id
                                       and sessions.user_name = ?)
           and instances.status != 'Other'