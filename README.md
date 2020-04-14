# Settlers-of-Catan
The optimal starting postion in Settlers of Catan has many factors that affect the choice.
Using probability, resource scarcity and the ability to trade has been used to identify a possible choice.

You will need to place the Dataset, Pathway and Probabilites in your working directory.

The following R libraries are need to run this program:

igraph

dplyr

# Setting Up The Board
First update Dataset-NODES.csv
You only need to change the first 19 lines.
Refer to BlankBoard.jpg for the labeling of the board.

Only change name.type, type.label, and name.roll

1	Desert

2	Brick

3	Grain

4	Lumber

5	Ore

6	Wool

EXAMPLE Dataset-NODE.csv:

id	name	name.type	type.label	name.roll

A	ResourceA	4	Lumber	8

B	ResourceB	6	Wool	11

C	ResourceC	4	Lumber	6

D	ResourceD	6	Wool	4

E	ResourceE	3	Grain	2

F	ResourceF	5	Ore	10

G	ResourceG	3	Grain	4

H	ResourceH	6	Wool	5

I	ResourceI	6	Wool	11

J	ResourceJ	5	Ore	9

K	ResourceK	3	Grain	5

L	ResourceL	5	Ore	3

M	ResourceM	4	Lumber	8

N	ResourceN	2	Brick	3

O	ResourceO	3	Grain	9

P	ResourceP	4	Lumber	10

Q	ResourceQ	1	Desert	7

R	ResourceR	2	Brick	6

S	ResourceS	2	Brick	12


# Adding in the Ports
Update the Pathway-NODES.csv
This is the network that looks at the pathways to the ports.

You only need to update the first 9 lines for trade.type and trade.label

1	Everything

2	Brick

3	Grain

4	Lumber

5	Ore

6	Wool


EXAMPLE Pathway-NODES.csv

id	name	trade.type	trade.label

P1	P1	3	Grain

P2	P2	4	Lumber

P3	P3	5	Ore

P4	P4	1	Everything

P5	P5	6	Wool

P6	P6	1	Everything

P7	P7	1	Everything

P8	P8	2	Brick

P9	P9	1	Everything







