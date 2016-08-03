-- ACL ----
-- | Delete a calendar
delete :: CalendarID -> RequestM ()
-- | Get a calendar by id
get :: CalendarID -> RequestM Calendar
-- | Insert
insert :: [CalendarList] -> CalendarID -> RequestM ()
-- | List calendars
list :: Params -> RequestM [Calendar]
-- | Updates a resource in a calendar
patch  :: CalendarID -> RequestM ()
-- | Updates a resource in a calendar
update :: CalendarID -> RequestM ()
-- | Watch for changes to calendarlist resources
watch :: a -> RequestM ()

