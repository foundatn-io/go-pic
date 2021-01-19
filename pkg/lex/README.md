# PIC clauses in Copybooks

## What does a line in a copybook look like?
Basic info below sourced from [tutorialspoint](https://www.tutorialspoint.com/cobol/cobol_data_types.htm).
```
01            TOTAL-STUDENTS            PIC9(5)            VALUE '125'.
|                    |                    |                    |
|                    |                    |                    |
|                    |                    |                    | 
Level Number     Data Name           Picture Clause       Value Clause
```

### Level Number
Level number is used to specify the level of data in a record. They are used for differentiating between elementary items and group items. Elementary items can be grouped together to create group items.

Level numbers adhere to the following rules:

| Sr.No.   | Level Number & Description 
|----------|----------------------------
| 01       | Record description entry
| 02 - 49  | Group and Elementary items
| 66       | Rename Clause items
| 77       | Items which cannot be sub-divided
| 88       | Condition name entry

- Elementary items cannot be divided further. Level number, Data name, Picture clause, and Value clause (optional) are used to describe an elementary item.

- Group items consist of one or more elementary items. Level number, Data name, and Value clause (optional) are used to describe a group item. Group level number is always 01

### Data Name
 Data names must be defined in the Data Division before using them in the Procedure Division. They must have a user-defined name; reserved words cannot be used. Data names give reference to the memory locations where actual data is stored. They can be elementary or group type.
 
### Picture clause

A Picture clause is used to define the following items:

- Data type can be numeric, alphabetic, or alphanumeric. Numeric type consists of only digits 0 to 9. Alphabetic type consists of letters A to Z and spaces. Alphanumeric type consists of digits, letters, and special characters.

- Sign can be used with numeric data. It can be either + or –.

- Decimal point position can be used with numeric data. Assumed position is the position of decimal point and not included in the data.
    
- Length defines the number of bytes used by the data item.

| Symbol |  Description 
|--------|----------------------------
| 9      | Numeric
| A      | Alphabetic
| X      | Alphanumeric
| V      | Implicit decimal
| S      | Sign
| P      | Assumed decimal place

## Possible PIC definitions
Basic:
 - `PIC X.`
 - `PIC 9999.`
 - `PIC X(4).`
 
Mixed:
 - `PIC S99V99`
 - `PIC S9(3)V9(2).`
 - `PIC PPP999.`
 
### Determining applicable Go Types
`i` - Int  
`u` - Uint  
`s` - String  

| *X* | *9* | *A* | *X* | *V* | *S* | *P* 
|-----|-----|-----|-----|-----|-----|-----
| *9* | u   | s   | s   | u   | i   | u 
| *A* | s   | s   | s   | s   | s   | s 
| *X* | s   | s   | s   | s   | s   | s 
| *V* | u   | s   | s   | ?   | i   | u 
| *S* | i   | s   | s   | i   | i   | i 
| *P* | u   | s   | s   | u   | i   | u 

### Applying regex
PICs can essentially be comprised of any pattern of the above symbols

`S9(5)VXXXXA(12)` is a valid definition for `-12345.AB12DABCDEFGHIJKL`

This quickly becomes difficult to handle with regex. So support for basic patterns will be created

Signed Numbers:
 - `PIC S9(3)V9(2).`

Decimals:
 - `PIC PPP999.`


