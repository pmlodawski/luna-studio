select * from instances
         where ? >= (select count (*) from sessions
                                      where sessions.instance_id = instances.id)