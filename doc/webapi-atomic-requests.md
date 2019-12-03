# Atomicity of PostgreSQL Interactions

Both the webapi and the node which populates the database operate on the database within a
database transaction. All operations on the database from Haskell code is done in a function
which has a type signatures of :
```
ReaderT SqlBackend m a
```
Any function without the `ReaderT SqlBackend` component will not be able to access the database
and any attempt to access the database without the required type signature will result in a compile
error at compile time.

All functions with the required file type are run with the function provided by Haskell's
[Persistent][Persistent] library:
```
runSqlConnWithIsolation action backend Serializable
```
where:
* `runSqlConnWithIsolation` is the function that runs the provided `action` on a connection to
  the database within a database transaction.
* `action` is the action to be performed on the database (eg write or query).
* `backend` contains the database connection data.
* `Serializable` specifies the transaction isolation level.

In this case the `Serializable` [transaction isolation][PosgresIso] level is used which is the
*maximum* level of transaction isolation.

[Persistent]: https://hackage.haskell.org/package/persistent
[PosgresIso]: https://www.postgresql.org/docs/current/transaction-iso.html

