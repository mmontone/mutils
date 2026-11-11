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


### call-with-transaction

```lisp
(func &optional (transaction-class 'transaction))
```


### commit

```lisp
()
```


### commit-transaction

```lisp
(transaction)
```


### current-transaction

```lisp
()
```


### rollback

```lisp
()
```


### rollback-transaction

```lisp
(transaction)
```


## Macros
### with-transaction

```lisp
((&optional transaction-class) &body body)
```



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
### transactional-object
Changes of a transactional object are placed in the current transaction.

