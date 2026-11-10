# transactional-object

Memory transactions for CLOS objects.

[[source code]](../transactional-object.lisp)

- **Author**: Mariano Montone
- **Version**: 0.1
- **Requires**: closer-mop

 TODO
 


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
### \*transactions\*
A list of current transactions

## Classes
### transaction
### transactional-class
### transactional-object
Changes of a transactional object are placed in the current transaction.

