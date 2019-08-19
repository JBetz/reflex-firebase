Firebase bindings for `reflex-dom`. Useful for rapid prototyping with a cloud-hosted database.

## Cheat Sheet
```haskell

-- Read
query        :: Query q r -> Firebase m [r]

-- Write
add          :: q r -> Event t r -> Firebase m ()

-- Meta-read
subscribe    :: Query q r -> Firebase m (Dynamic t [r])
dynSubscribe :: Dynamic t (Query q r) -> Firebase m (Dynamic t [r])
```

### Example
```haskell
do
  let route = Route_RoomMessages "#reflex-frp"
      params = [OrderBy "time_sent" Asc, Limit 100]
  dynMessages <- subscribe (Query route params)
  el "ul" $ simpleList (\message -> el "li" $ text $ view content message) messagesD
```

## Todo
- implement all read queries
- implement all write queries
- create DSL for Firebase rules
- integrate Firebase rules with routes
- support Firebase auth
- use document change events for incremental view updates
- remove dependence on Javascript SDK by calling Firebase API directly
