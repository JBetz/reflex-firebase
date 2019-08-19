Firebase bindings for `reflex-dom`. Useful for rapid prototyping with a cloud-hosted database.

### Setup
1. Firebase: [setup instructions](https://firebase.google.com/docs/web/setup)
1. Reflex: use [obelisk](https://github.com/obsidiansystems/obelisk) for optimum productivity. See instructions for adding package overrides.

Note: importing the Javascript SDK is required until it is replaced with direct calls to the Firebase API

### Cheat Sheet
```haskell
-- All events are run in MonadFirebase, which holds a reference to a Firebase application object

-- Read
query        :: Route q r => Query q r -> m [r]

-- Write
add          :: Route q r => q r -> Event t r -> m ()
update       :: (Route q r, HasId r) => q r -> Event t r -> m ()
set          :: (Route q r, HasId r) => q r -> Event t r -> m ()
delete       :: (Route q r, HasId r) => q r -> Event t r -> m ()

-- Meta-read
subscribe    :: Route q r => Query q r -> m (Dynamic t [r])
dynSubscribe :: Route q r => Dynamic t (Query q r) -> m (Dynamic t [r])
```

#### Example
```haskell
do
  let route = Route_RoomMessages "#reflex-frp"
      params = [OrderBy "time_sent" Asc, Limit 100]
  dynMessages <- subscribe (Query route params)
  el "ul" $ simpleList (\message -> el "li" $ text $ view content message) messagesD
```

### Todo
- read queries
- write queries
- error handling
- use document change events for incremental view updates
- DSL for Firebase rules
- integrate Firebase rules with routes
- support Firebase auth
