------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2022, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

--  <description>
--  A `GOptionContext` struct defines which options are accepted by the
--  commandline option parser. The struct has only private fields and should
--  not be directly accessed.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Glib.Application;        use Glib.Application;
with Glib.Error;              use Glib.Error;
with Glib.Generic_Properties; use Glib.Generic_Properties;
with Gtkada.Bindings;         use Gtkada.Bindings;
with Gtkada.Types;            use Gtkada.Types;

package Glib.Option is

   type Goption_Context is new Glib.C_Boxed with null record;
   Null_Goption_Context : constant Goption_Context;

   function From_Object (Object : System.Address) return Goption_Context;
   function From_Object_Free (B : access Goption_Context'Class) return Goption_Context;
   pragma Inline (From_Object_Free, From_Object);

   type GOption_Error is (
      G_Option_Error_Unknown_Option,
      G_Option_Error_Bad_Value,
      G_Option_Error_Failed);
   pragma Convention (C, GOption_Error);
   --  Error codes returned by option parsing.

   type GOption_Arg is (
      G_Option_Arg_None,
      G_Option_Arg_String,
      G_Option_Arg_Int,
      G_Option_Arg_Callback,
      G_Option_Arg_Filename,
      G_Option_Arg_String_Array,
      G_Option_Arg_Filename_Array,
      G_Option_Arg_Double,
      G_Option_Arg_Int64);
   pragma Convention (C, GOption_Arg);
   --  The Glib.Option.GOption_Arg enum values determine which type of extra
   --  argument the options expect to find. If an option expects an extra
   --  argument, it can be specified in several ways; with a short option: `-x
   --  arg`, with a long option: `--name arg` or combined in a single argument:
   --  `--name=arg`.

   type GOption_Flags is mod 2 ** Integer'Size;
   pragma Convention (C, GOption_Flags);
   --  Flags which modify individual options.

   G_Option_Flag_None : constant GOption_Flags := 0;
   G_Option_Flag_Hidden : constant GOption_Flags := 1;
   G_Option_Flag_In_Main : constant GOption_Flags := 2;
   G_Option_Flag_Reverse : constant GOption_Flags := 4;
   G_Option_Flag_No_Arg : constant GOption_Flags := 8;
   G_Option_Flag_Filename : constant GOption_Flags := 16;
   G_Option_Flag_Optional_Arg : constant GOption_Flags := 32;
   G_Option_Flag_Noalias : constant GOption_Flags := 64;

   type GOption_Group is new Glib.C_Proxy;
   function From_Object_Free (B : access GOption_Group) return GOption_Group;
   pragma Inline (From_Object_Free);
   --  A `GOptionGroup` struct defines the options in a single group. The
   --  struct has only private fields and should not be directly accessed.
   --
   --  All options in a group share the same translation function. Libraries
   --  which need to parse commandline options are expected to provide a
   --  function for getting a `GOptionGroup` holding their options, which the
   --  application can then add to its Glib.Option.Goption_Context.

   type GOption_Entry is record
      Long_Name : Gtkada.Types.Chars_Ptr;
      Short_Name : Gchar;
      Flags : GOption_Flags;
      Arg : GOption_Arg;
      Arg_Data : System.Address := System.Null_Address;
      Description : Gtkada.Types.Chars_Ptr;
      Arg_Description : Gtkada.Types.Chars_Ptr;
   end record;
   pragma Convention (C, GOption_Entry);

   function From_Object_Free (B : access GOption_Entry) return GOption_Entry;
   pragma Inline (From_Object_Free);
   --  A GOptionEntry struct defines a single option. To have an effect, they
   --  must be added to a Glib.Option.GOption_Group with
   --  Glib.Option.Add_Main_Entries or g_option_group_add_entries.

   type GOption_Entry_Array is array (Natural range <>) of GOption_Entry;

   ---------------
   -- Callbacks --
   ---------------

   type Gtranslate_Func is access function (Str : UTF8_String) return UTF8_String;
   --  The type of functions which are used to translate user-visible strings,
   --  for <option>--help</option> output.
   --  "str": the untranslated string

   ----------------------------
   -- Enumeration Properties --
   ----------------------------

   package GOption_Error_Properties is
      new Generic_Internal_Discrete_Property (GOption_Error);
   type Property_GOption_Error is new GOption_Error_Properties.Property;

   package GOption_Arg_Properties is
      new Generic_Internal_Discrete_Property (GOption_Arg);
   type Property_GOption_Arg is new GOption_Arg_Properties.Property;

   package GOption_Flags_Properties is
      new Generic_Internal_Discrete_Property (GOption_Flags);
   type Property_GOption_Flags is new GOption_Flags_Properties.Property;

   -------------
   -- Methods --
   -------------

   procedure Add_Group (Self : Goption_Context; Group : GOption_Group);
   --  Adds a Glib.Option.GOption_Group to the Context, so that parsing with
   --  Context will recognize the options in the group. Note that the group
   --  will be freed together with the context when Glib.Option.Free is called,
   --  so you must not free the group yourself after adding it to a context.
   --  Since: gtk+ 2.6
   --  "group": the group to add

   procedure Add_Main_Entries
      (Self               : Goption_Context;
       Entries            : GOption_Entry_Array;
       Translation_Domain : UTF8_String := "");
   --  A convenience function which creates a main group if it doesn't exist,
   --  adds the Entries to it and sets the translation domain.
   --  Since: gtk+ 2.6
   --  "entries": a null-terminated array of GOption_Entrys
   --  "translation_domain": a translation domain to use for translating the
   --  `--help` output for the options in Entries with gettext, or null

   procedure Free (Self : Goption_Context);
   --  Frees context and all the groups which have been added to it.
   --  Please note that parsed arguments need to be freed separately (see
   --  Glib.Option.GOption_Entry).
   --  Since: gtk+ 2.6

   function Get_Description (Self : Goption_Context) return UTF8_String;
   --  Returns the description. See Glib.Option.Set_Description.
   --  Since: gtk+ 2.12

   procedure Set_Description
      (Self        : Goption_Context;
       Description : UTF8_String := "");
   --  Adds a string to be displayed in `--help` output after the list of
   --  options. This text often includes a bug reporting address.
   --  Note that the summary is translated (see
   --  Glib.Option.Set_Translate_Func).
   --  Since: gtk+ 2.12
   --  "description": a string to be shown in `--help` output after the list
   --  of options, or null

   function Get_Help
      (Self      : Goption_Context;
       Main_Help : Boolean;
       Group     : GOption_Group) return UTF8_String;
   --  Returns a formatted, translated help text for the given context. To
   --  obtain the text produced by `--help`, call `g_option_context_get_help
   --  (context, TRUE, NULL)`. To obtain the text produced by `--help-all`,
   --  call `g_option_context_get_help (context, FALSE, NULL)`. To obtain the
   --  help text for an option group, call `g_option_context_get_help (context,
   --  FALSE, group)`.
   --  Since: gtk+ 2.14
   --  "main_help": if True, only include the main group
   --  "group": the Glib.Option.GOption_Group to create help for, or null

   function Get_Help_Enabled (Self : Goption_Context) return Boolean;
   --  Returns whether automatic `--help` generation is turned on for Context.
   --  See Glib.Option.Set_Help_Enabled.
   --  Since: gtk+ 2.6

   procedure Set_Help_Enabled
      (Self         : Goption_Context;
       Help_Enabled : Boolean);
   --  Enables or disables automatic generation of `--help` output. By
   --  default, g_option_context_parse recognizes `--help`, `-h`, `-?`,
   --  `--help-all` and `--help-groupname` and creates suitable output to
   --  stdout.
   --  Since: gtk+ 2.6
   --  "help_enabled": True to enable `--help`, False to disable it

   function Get_Ignore_Unknown_Options
      (Self : Goption_Context) return Boolean;
   --  Returns whether unknown options are ignored or not. See
   --  Glib.Option.Set_Ignore_Unknown_Options.
   --  Since: gtk+ 2.6

   procedure Set_Ignore_Unknown_Options
      (Self           : Goption_Context;
       Ignore_Unknown : Boolean);
   --  Sets whether to ignore unknown options or not. If an argument is
   --  ignored, it is left in the Argv array after parsing. By default,
   --  g_option_context_parse treats unknown options as error.
   --  This setting does not affect non-option arguments (i.e. arguments which
   --  don't start with a dash). But note that GOption cannot reliably
   --  determine whether a non-option belongs to a preceding unknown option.
   --  Since: gtk+ 2.6
   --  "ignore_unknown": True to ignore unknown options, False to produce an
   --  error when unknown options are met

   function Get_Main_Group (Self : Goption_Context) return GOption_Group;
   --  Returns a pointer to the main group of Context.
   --  Since: gtk+ 2.6

   procedure Set_Main_Group (Self : Goption_Context; Group : GOption_Group);
   --  Sets a Glib.Option.GOption_Group as main group of the Context. This has
   --  the same effect as calling Glib.Option.Add_Group, the only difference is
   --  that the options in the main group are treated differently when
   --  generating `--help` output.
   --  Since: gtk+ 2.6
   --  "group": the group to set as main group

   function Get_Summary (Self : Goption_Context) return UTF8_String;
   --  Returns the summary. See Glib.Option.Set_Summary.
   --  Since: gtk+ 2.12

   procedure Set_Summary
      (Self    : Goption_Context;
       Summary : UTF8_String := "");
   --  Adds a string to be displayed in `--help` output before the list of
   --  options. This is typically a summary of the program functionality.
   --  Note that the summary is translated (see Glib.Option.Set_Translate_Func
   --  and Glib.Option.Set_Translation_Domain).
   --  Since: gtk+ 2.12
   --  "summary": a string to be shown in `--help` output before the list of
   --  options, or null

   procedure Set_Translate_Func
      (Self           : Goption_Context;
       Func           : Gtranslate_Func;
       Destroy_Notify : Glib.G_Destroy_Notify_Address);
   --  Sets the function which is used to translate the contexts user-visible
   --  strings, for `--help` output. If Func is null, strings are not
   --  translated.
   --  Note that option groups have their own translation functions, this
   --  function only affects the Parameter_String (see Glib.Option.G_New), the
   --  summary (see Glib.Option.Set_Summary) and the description (see
   --  Glib.Option.Set_Description).
   --  If you are using gettext, you only need to set the translation domain,
   --  see Glib.Option.Set_Translation_Domain.
   --  Since: gtk+ 2.12
   --  "func": the Gtranslate_Func, or null
   --  "destroy_notify": a function which gets called to free Data, or null

   generic
      type User_Data_Type (<>) is private;
      with procedure Destroy (Data : in out User_Data_Type) is null;
   package Set_Translate_Func_User_Data is

      type Gtranslate_Func is access function
        (Str  : UTF8_String;
         Data : User_Data_Type) return UTF8_String;
      --  The type of functions which are used to translate user-visible strings,
      --  for <option>--help</option> output.
      --  "str": the untranslated string
      --  "data": user data specified when installing the function, e.g. in
      --  g_option_group_set_translate_func

      procedure Set_Translate_Func
         (Self           : Glib.Option.Goption_Context;
          Func           : Gtranslate_Func;
          Data           : User_Data_Type;
          Destroy_Notify : Glib.G_Destroy_Notify_Address);
      --  Sets the function which is used to translate the contexts
      --  user-visible strings, for `--help` output. If Func is null, strings
      --  are not translated.
      --  Note that option groups have their own translation functions, this
      --  function only affects the Parameter_String (see Glib.Option.G_New),
      --  the summary (see Glib.Option.Set_Summary) and the description (see
      --  Glib.Option.Set_Description).
      --  If you are using gettext, you only need to set the translation
      --  domain, see Glib.Option.Set_Translation_Domain.
      --  Since: gtk+ 2.12
      --  "func": the Gtranslate_Func, or null
      --  "data": user data to pass to Func, or null
      --  "destroy_notify": a function which gets called to free Data, or null

   end Set_Translate_Func_User_Data;

   procedure Set_Translation_Domain
      (Self   : Goption_Context;
       Domain : UTF8_String);
   --  A convenience function to use gettext for translating user-visible
   --  strings.
   --  Since: gtk+ 2.12
   --  "domain": the domain to use

   ----------------------
   -- GtkAda additions --
   ----------------------

   Null_GOption_Entry : constant GOption_Entry;

   type Parse_Filter is access function (Param : String) return Boolean;
   --  Returns True if the parameter is to be passed from Command_Line to
   --  Goption_Context

   procedure Parse
     (Self         : Goption_Context;
      Command_Line : not null access Glib.Application.Gapplication_Command_Line_Record'Class;
      Filter       : Parse_Filter := null;
      Success      : out Boolean;
      Error        : out Glib.Error.GError);
   --  Parses the arguments given via Command_Line, removing from the arguments
   --  list all parsed switches.

   procedure Parse
     (Self         : Goption_Context;
      Argv         : access chars_ptr_array_access;--  Null-terminated
      Success      : out Boolean;
      Error        : out Glib.Error.GError);
   --  This version is suitable for use from Glib.Application.Local_Command_Line

   ---------------
   -- Functions --
   ---------------

   function G_New
      (Parameter_String : UTF8_String := "") return Goption_Context;
   --  Creates a new option context.
   --  The Parameter_String can serve multiple purposes. It can be used to add
   --  descriptions for "rest" arguments, which are not parsed by the
   --  Glib.Option.Goption_Context, typically something like "FILES" or "FILE1
   --  FILE2...". If you are using G_OPTION_REMAINING for collecting "rest"
   --  arguments, GLib handles this automatically by using the Arg_Description
   --  of the corresponding Glib.Option.GOption_Entry in the usage summary.
   --  Another usage is to give a short summary of the program functionality,
   --  like " - frob the strings", which will be displayed in the same line as
   --  the usage. For a longer description of the program functionality that
   --  should be displayed as a paragraph below the usage line, use
   --  Glib.Option.Set_Summary.
   --  Note that the Parameter_String is translated using the function set
   --  with Glib.Option.Set_Translate_Func, so it should normally be passed
   --  untranslated.
   --  Since: gtk+ 2.6
   --  "parameter_string": a string which is displayed in the first line of
   --  `--help` output, after the usage summary `programname [OPTION...]`

private

   Null_Goption_Context : constant Goption_Context := (Glib.C_Boxed with null record);


   Null_GOption_Entry : constant GOption_Entry :=
                          (Gtkada.Types.Null_Ptr, Gchar(ASCII.NUL),
                           0, G_Option_Arg_None, System.Null_Address,
                           Gtkada.Types.Null_Ptr, Gtkada.Types.Null_Ptr);
       
end Glib.Option;
