*******************************************************************
Using GtkParasite to inspect and modify running GtkAda applications
*******************************************************************

GtkParasite is a tool that enables one to inspect running Gtk+ applications. It’s usable with GtkAda application without any Ada specific recommendations.

When it is installed, you have to make sure that your application will be able to access GtkParasite library. This means either :

* Linking against the GtkParasite library during compilation of your application.
* Make sure that the dynamic library file is accessible at runtime, for example by adding its path to the LD_LIBRARY_PATH environnment variable before running your application.

Then, to run GtkParasite with your application::

    $ GTK_MODULES=gtkparasite yourapp

You can find more information about how to use GtkParasite on it’s web page here : `GtkParasite Home`_

.. _`GtkParasite Home`: http://chipx86.github.com/gtkparasite/
