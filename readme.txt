4/15
I began to implement some of the shapes, Earlier you mentioned a complete redo of the way our enviorment was going to be implemented.

For this reason I don't feel it wise to set up proper calls to each shape until then.

This should be considered drop-in code however I "dropped" it into the main code so you could have an idea of how it will work.

There are still some bugs to work out, by just dropping it in without proper implementation it attempts to draw all the shapes at once. You can see if you care to, by running main gui and events. I think the other problem is with make-perfect and the line appears to be acting wonky but I havnt looked into that yet.

Some good news, ellipse and rectangle seem to be working properly so I'm hoping the others wont be too difficult to fix. rectangle does not render unless you comment out the other shapes.

I have also  updated gui and events, I changed buttons to trigger proper events


update 4/16 Fixed the make-perfect proc using objects and dispatch
