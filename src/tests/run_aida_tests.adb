with Ahven.Framework;
with Ahven.Text_Runner;
--with Application_Locator;
with Aida.Tests;
with Aida.UTF8.Tests;
with Aida.Bounded_String_Tests;
with Aida.Bounded_Hash_Map_Tests;
with Aida.XML_SAX_Parse_Tests;
with Aida.XML_DOM_Parser_Tests;
with Aida.JSON_SAX_Parse_Tests;
with Aida.JSON_DOM_Parser_Tests;
with Aida.Subprogram_Call_Result_Tests;
with Aida.Integer_To_String_Map_Tests;

procedure Run_Aida_Tests is
   S : aliased Ahven.Framework.Test_Suite
     := Ahven.Framework.Create_Suite ("All");

   Converstion_Test : Aida.Tests.Test;

   Bounded_String_Test : Aida.Bounded_String_Tests.Test;

   Bounded_Hash_Map_Test : Aida.Bounded_Hash_Map_Tests.Test;

   XML_Parsing_Test : Aida.XML_SAX_Parse_Tests.Test;

   XML_DOM_Parser_Test : Aida.XML_DOM_Parser_Tests.Test;

   JSON_Parsing_Test : Aida.JSON_SAX_Parse_Tests.Test;

   JSON_DOM_Parsing_Test : Aida.JSON_DOM_Parser_Tests.Test;

   Subprogram_Call_Test : Aida.Subprogram_Call_Result_Tests.Test;

   UTF8_Test : Aida.UTF8.Tests.Test;

   Integer_To_String_Test : Aida.Integer_To_String_Map_Tests.Test;
begin
   Ahven.Framework.Add_Static_Test (S, Bounded_String_Test);
   Ahven.Framework.Add_Static_Test (S, Bounded_Hash_Map_Test);
   Ahven.Framework.Add_Static_Test (S, Converstion_Test);
   Ahven.Framework.Add_Static_Test (S, UTF8_Test);
   Ahven.Framework.Add_Static_Test (S, XML_Parsing_Test);
   Ahven.Framework.Add_Static_Test (S, XML_DOM_Parser_Test);
   Ahven.Framework.Add_Static_Test (S, JSON_Parsing_Test);
   Ahven.Framework.Add_Static_Test (S, JSON_DOM_Parsing_Test);
   Ahven.Framework.Add_Static_Test (S, Subprogram_Call_Test);
   Ahven.Framework.Add_Static_Test (S, Integer_To_String_Test);
   Ahven.Text_Runner.Run (S);
end Run_Aida_Tests;
