Firebase bindings for `reflex-dom`. Useful for rapid prototyping with a cloud-hosted database. Originally a proof of concept, the goal is to become production ready, and to utilize the best of Haskell's type system to better manage Firebase's NoSQL document store.

### Setup
1. Firebase: [setup instructions](https://firebase.google.com/docs/web/setup)
1. Reflex: use [obelisk](https://github.com/obsidiansystems/obelisk) for optimum productivity. See instructions for adding package overrides.

Note: importing the Javascript SDK is required until it is replaced with direct calls to the Firebase API

### Cheat Sheet
```haskell
-- All events are run in MonadFirebase, which holds a reference to a Firebase application object

-- Read
query        :: Query q r -> m [r]

-- Write
add          :: q r -> Event t r -> m ()
update       :: _
set          :: _
delete       :: _

-- Meta-read
subscribe    :: Query q r -> m (Dynamic t [r])
dynSubscribe :: Dynamic t (Query q r) -> m (Dynamic t [r])
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
