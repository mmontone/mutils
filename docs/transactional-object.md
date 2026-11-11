# transactional-object

Memory transactions for CLOS objects.

[[source code]](../transactional-object.lisp)

- **Author**: Mariano Montone
- **Version**: 0.1
- **Requires**: closer-mop


 A simple implementation of memory transactions for CLOS objects.
 Slot access is intercepted and values are stored and retrieved
 from the current dynamically bound transaction.
 There's no support for versioning and concurrency. For that see the project: https://github.com/mmontone/transactional-object
 This library can be useful for implementing the editing of object from a user interface with save and cancel buttons, without having to copy data between objects. Start a transaction when the editing start, commit the transaction on save, or rollback on cancel.



## Functions
### begin

```lisp
(&optional (transaction-class 'transaction))
```

Create a new transaction and make it active.
Returns the created transaction.




### call-with-transaction

```lisp
(func &optional (transaction-class 'transaction))
```

Create a new transaction and call FUNC with that transaction as current.




### commit

```lisp
()
```

Commit current transaction.




### commit-transaction

```lisp
(transaction)
```

Commit TRANSACTION.




### current-transaction

```lisp
()
```

Returns the current active transaction.




### rollback

```lisp
()
```

Rollback current transaction.




### rollback-transaction

```lisp
(transaction)
```

Rollback TRANSACTION.




## Macros
### with-transaction

```lisp
((&optional transaction-class) &body body)
```

Create a transaction and evaluate BODY with that transaction as current.





## Slot-Accessors
### transaction-status
The transaction status. May be :new, :commited, :failed, etc

### (setf transaction-status)
The transaction status. May be :new, :commited, :failed, etc

### transaction-values
A hash table with modified objects as keys and slot values as values.

### (setf transaction-values)
A hash table with modified objects as keys and slot values as values.

## Variables
### \*autocommit\*
When enabled, the transaction is commited when the scope of a transaction ends.

### \*transactions\*
A list of current transactions

## Classes
### transaction
### transactional-class
Metaclass for classes which instances can be part of transactions.

### transactional-object
Changes of a transactional object are placed in the current transaction.

