--  Gtkada.Intl tests for gettext fallback, text domains, and locale access.
--  No matching gettext C test is present in testsuite/c_tests.

with Ada.Command_Line;

with Glib.Test; use Glib.Test;
with Gtkada.Intl; use Gtkada.Intl;

procedure Main is

   procedure Test_Internationalization
   with Convention => C;

   -------------------------------
   -- Test_Internationalization --
   -------------------------------

   procedure Test_Internationalization is
      Domain : constant String := "gtkada-intl-test";
   begin
      --  The C locale is available on every supported host and makes gettext
      --  return the original message when no translation catalog is present.
      Setlocale (LC_ALL, "C");
      Assert_Cmpstr_Eq (Getlocale, "C");

      --  The default, explicit-domain, category-specific, and shorthand
      --  lookups all preserve an untranslated message.
      Assert_Cmpstr_Eq (Gettext ("untranslated message"), "untranslated message");
      Assert_Cmpstr_Eq
        (Dgettext (Domain, "domain message"), "domain message");
      Assert_Cmpstr_Eq
        (Dcgettext (Domain, "category message", LC_MESSAGES),
         "category message");
      Assert_Cmpstr_Eq (-"shorthand message", "shorthand message");

      --  Binding a domain and its codeset is observable through subsequent
      --  lookups; with no catalog installed, lookup still returns its msgid.
      Bind_Text_Domain (Domain, "/no-such-gtkada-intl-catalog");
      Bind_Text_Domain_Codeset (Domain, "UTF-8");
      Assert_Cmpstr_Eq
        (Dgettext (Domain, "still untranslated"), "still untranslated");

      --  Preserve the process's initial domain and verify both setting and
      --  restoring it through the query function.
      declare
         Original_Domain : constant String := Default_Text_Domain;
      begin
         Text_Domain (Domain);
         Assert_Cmpstr_Eq (Default_Text_Domain, Domain);
         Assert_Cmpstr_Eq (Gettext ("default-domain message"),
                           "default-domain message");
         Text_Domain (Original_Domain);
         Assert_Cmpstr_Eq (Default_Text_Domain, Original_Domain);
      end;
   end Test_Internationalization;

begin
   Glib.Test.Init;
   Glib.Test.Add_Func
     ("/gtkada-intl/gettext", Test_Internationalization'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
