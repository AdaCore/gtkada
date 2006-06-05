var current_page='';

function switchPage (name) {
  if (current_page == name) {
     return true;
  }
  current_page = name;

  var children =
     document.getElementById ('documentation').getElementsByTagName ('div');
  for (var i=0; i < children.length; i++) {
     if (children[i].id == 'notebook_' + name) {
        children[i].style.display = 'block';
     } else if (children[i].className == 'notebookPage') {
     children[i].style.display = 'none';
     }
  }

  children = document.getElementById ('notebook').getElementsByTagName ('li');
  for (i=0; i < children.length; i++) {
     if (children[i].id == 'tab_' + name) {
         children[i].className = 'current';
     } else {
         children[i].className = '';
     }
  }

  return true;
}

/* Return the window's height */

function getWindowHeight() {
  var wh=0;
  if ( typeof window.innerHeight != 'undefined')
    wh = window.innerHeight;
  else {
    d = document;
   if ( d.documentElement
       && typeof d.documentElement.clientHeight != 'undefined'
       && d.documentElement.clientHeight != 0)
     wh = d.documentElement.clientHeight;
   else if (d.body
            && typeof d.body.clientHeight != 'undefined' )
     wh = d.body.clientHeight;
   else
     alert ("Can't identify window height")
  }
  return wh;
}

/* Adjust the height of various elements to adapt to the screen size */
function adjust_height() {
   /* Do nothing at present (see comment about automatic scrollbars in
      gtkada_rm.css */
   return;

   var screenHeight = getWindowHeight();

   var objectName = document.getElementById ('objectName');
   screenHeight = screenHeight - objectName.clientHeight - 10;

   var rightSide = document.getElementById ('rightSide');
   var h = rightSide.getElementsByTagName ('h2');
   screenHeight = screenHeight - h[0].clientHeight - 20;

   /* The following code works with Opera, but not firefox */
   var ul = rightSide.getElementsByTagName ('ul');
   ul[0].style.maxHeight = screenHeight;
   ul[0].style.height = screenHeight;

   var divs = document.getElementsByTagName ('div');
   for (var n=0; n < divs.length; n++) {
       if (divs[n].className == 'notebookPage') {
          divs[n].style.maxHeight = screenHeight;
          divs[n].style.height    = screenHeight;
       }
   }
}
