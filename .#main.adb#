-- A skeleton of a program for an assignment in programming languages
-- The students should rename the tasks of producers, consumers, and the buffer
-- Then, they should change them so that they would fit their assignments
-- They should also complete the code with constructions that lack there
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Numerics.Discrete_Random;


procedure Simulation is

   Number_Of_Products: constant Integer := 5;
   Number_Of_Assemblies: constant Integer := 3;
   Number_Of_Consumers: constant Integer := 2;
   subtype Product_Type is Integer range 1 .. Number_Of_Products;
   subtype Assembly_Type is Integer range 1 .. Number_Of_Assemblies;
   subtype Consumer_Type is Integer range 1 .. Number_Of_Consumers;
   Product_Name: constant array (Product_Type) of String(1 .. 8)
     := ("Paczek  ", "Donut   ", "Tiramisu", "Babeczka", "Kawa    ");
   Assembly_Name: constant array (Assembly_Type) of String(1 .. 13)
     := ("Z.sniadaniowy", "Z.lunchowy   ", "Z.konesera   ");
   package Random_Assembly is new
     Ada.Numerics.Discrete_Random(Assembly_Type);
   type My_Str is new String(1 ..256);
   Someone_Using_Lada : Boolean := False;

   -- Producer produces determined product
   task type Producer is
      -- Give the Producer an identity, i.e. the product type
      entry Start(Product: in Product_Type; Production_Time: in Integer);
   end Producer;

   -- Consumer gets an arbitrary assembly of several products from the buffer
   task type Consumer is
      -- Give the Consumer an identity
      entry Start(Consumer_Number: in Consumer_Type;
		    Consumption_Time: in Integer);
   end Consumer;

   -- In the Buffer, products are assemblied into an assembly
   task type Buffer is
      -- Accept a product to the storage provided there is a room for it
      entry Take(Product: in Product_Type; Number: in Integer);
      -- Deliver an assembly provided there are enough products for it
      entry Deliver(Assembly: in Assembly_Type; Number: out Integer);
   end Buffer;

   P: array ( 1 .. Number_Of_Products ) of Producer;
   K: array ( 1 .. Number_Of_Consumers ) of Consumer;
   B: Buffer;

   task body Producer is
      subtype Production_Time_Range is Integer range 3 .. 5;
      package Random_Production is new
	Ada.Numerics.Discrete_Random(Production_Time_Range);
      G: Random_Production.Generator;	--  generator liczb losowych
      Product_Type_Number: Integer;
      Product_Number: Integer;
      Production: Integer;
   begin
      accept Start(Product: in Product_Type; Production_Time: in Integer) do
         Random_Production.Reset(G);	--  start random number generator
         Product_Number := 1;
         Product_Type_Number := Product;
         Production := Production_Time;
      end Start;
      Put_Line("Rozpoczeto gotowanie produktu: " & Product_Name(Product_Type_Number));
      loop
         delay Duration(Random_Production.Random(G)); --  simulate production

         Put_Line("Wytworzono " & Product_Name(Product_Type_Number)
                  & " nr "  & Integer'Image(Product_Number));
         -- Accept for storage
         B.Take(Product_Type_Number, Product_Number);
         Product_Number := Product_Number + 1;

      end loop;
   end Producer;

   task body Consumer is
      subtype Consumption_Time_Range is Integer range 4 .. 8;
      package Random_Consumption is new
	Ada.Numerics.Discrete_Random(Consumption_Time_Range);
      G: Random_Consumption.Generator;	--  random number generator (time)
      G2: Random_Assembly.Generator;	--  also (assemblies)
      Consumer_Nb: Consumer_Type;
      Assembly_Number: Integer;
      Consumption: Integer;
      Assembly_Type: Integer;
      Is_Waiting: Boolean;
      Consumer_Name: constant array (1 .. Number_Of_Consumers)
        of String(1 .. 7)
        := ("Klient1", "Klient2");
   begin
      accept Start(Consumer_Number: in Consumer_Type;
		     Consumption_Time: in Integer) do
         Random_Consumption.Reset(G);	--  ustaw generator

         Consumer_Nb := Consumer_Number;
         Consumption := Consumption_Time;
      end Start;
      Put_Line(Consumer_Name(Consumer_Nb) & " wchodzi do cukierni");
      loop
         delay Duration(Random_Consumption.Random(G)); --  simulate consumption
         Assembly_Type := Random_Assembly.Random(G2);
         if Someone_Using_Lada then
            Put_Line(Consumer_Name(Consumer_Nb) & " czeka w kolejce");
         else
            Someone_Using_Lada := True; -- ustawienie statusu poniewaz obecny konsumer uzywa lady
            Put_Line(Consumer_Name(Consumer_Nb) & " podchodzi do lady z checia kupna " & Assembly_Name(Assembly_Type) );
            -- take an assembly for consumption
            --select
               --S.Ask_For(Assembly_Type, Assembly_Number);
            delay Duration(0.5);

            B.Deliver(Assembly_Type, Assembly_Number);

            if Assembly_Number /= 0 then

               Put_Line(Consumer_Name(Consumer_Nb) & ": odchodzi zadowolony z " &
                          Assembly_Name(Assembly_Type) & " nr " &
                          Integer'Image(Assembly_Number));
            else
               Put_Line(Consumer_Name(Consumer_Nb) & " odchodzi od lady gdyz jego zestaw jest niedostepny ");
            end if;




            --or delay 1.0;
             --    S.Stop;
            --     Put_Line(Consumer_Name(Consumer_Nb) & " odchodzi od lady gdyz jego zestaw jest niedostepny ");
            --end select;

            Someone_Using_Lada := False; -- konsumer odchodzi od lady i przestaje jej uzywac
         end if;

      end loop;
   end Consumer;

   task body Buffer is
      Storage_Capacity: constant Integer := 30;
      type Storage_type is array (Product_Type) of Integer;
      Storage: Storage_type
        := (0, 0, 0, 0, 0);
      Assembly_Content: array(Assembly_Type, Product_Type) of Integer
        := ((0, 1, 0, 0, 2),   -- zestaw sniadaniowy
            (0, 0, 2, 0, 1),   -- zestaw lunchowy
            (0, 1, 1, 0, 0));  -- zestaw konesera
        --:= ((1, 1, 0, 0, 2),   -- zestaw sniadaniowy
        --    (1, 0, 2, 0, 1),   -- zestaw lunchowy
        --    (1, 1, 1, 1, 0));  -- zestaw konesera
      Max_Assembly_Content: array(Product_Type) of Integer;
      Assembly_Number: array(Assembly_Type) of Integer
        := (1, 1, 1);
      In_Storage: Integer := 0;

      procedure Setup_Variables is
      begin
         for W in Product_Type loop
            Max_Assembly_Content(W) := 0;
            for Z in Assembly_Type loop
               if Assembly_Content(Z, W) > Max_Assembly_Content(W) then
                  Max_Assembly_Content(W) := Assembly_Content(Z, W);
               end if;
            end loop;
         end loop;
      end Setup_Variables;


      function Can_Accept(Product: Product_Type) return Boolean is
         Free: Integer;		--  free room in the storage
                         -- how many products are for production of arbitrary assembly
         Lacking: array(Product_Type) of Integer;
         -- how much room is needed in storage to produce arbitrary assembly
         Lacking_room: Integer;
         MP: Boolean;			--  can accept
      begin
         if In_Storage >= Storage_Capacity then
            return False;
         end if;
         -- There is free room in the storage
         Free := Storage_Capacity - In_Storage;
         MP := True;

         for W in Product_Type loop
            if Storage(W) < Max_Assembly_Content(W) then
               MP := False;
            end if;
         end loop;

         if MP then
            return True;		--  storage has products for arbitrary
                          --  assembly
         end if;

         if Integer'Max(0, Max_Assembly_Content(Product) - Storage(Product)) > 0 then
            -- exactly this product lacks
            return True;
         end if;
         Lacking_room := 1;			--  insert current product
         for W in Product_Type loop
            Lacking(W) := Integer'Max(0, Max_Assembly_Content(W) - Storage(W));
            Lacking_room := Lacking_room + Lacking(W);
         end loop;
         if Free >= Lacking_room then
            -- there is enough room in storage for arbitrary assembly
            return True;
         else
            -- no room for this product
            return False;
         end if;
      end Can_Accept;

      function Can_Deliver(Assembly: Assembly_Type) return Boolean is
      begin
         for W in Product_Type loop
            if Storage(W) < Assembly_Content(Assembly, W) then
               return False;
            end if;
         end loop;
         return True;
      end Can_Deliver;

      procedure Storage_Contents is
      begin
         for W in Product_Type loop
            Put_Line("Na ladzie: " & Integer'Image(Storage(W)) & " "
                     & Product_Name(W));
         end loop;
      end Storage_Contents;

      procedure Remove_Excess is
         Over: array(Product_Type) of Integer;
         -- ile jest nadmiarowych sztuk produktu w odniesieniu do mozliwego kol zam
         Temp: Integer;
         Max: Integer := 0;
         Max_Product: Product_Type;

      begin
         -- wybranie produktu ktory zostanie usuniety
         -- produktu, ktorego obecna ilosc jest wystarczajaca do skompletowanai kazdego zestawu
            for W in Product_Type loop
               Temp := Storage(W) - Max_Assembly_Content(W);
            if Temp >= Max then
               Max := Temp;
               Max_Product := W;
            end if;
         end loop;

         Storage(Max_Product) := Storage(Max_Product) - 1;
         In_Storage := In_Storage - 1;
         Put_Line("Z lady usunieto 1 " & Product_Name(Max_Product));



      end Remove_Excess;


   begin
      Put_Line("Lada zaczyna przyjmowac produkty");
      Setup_Variables;
      loop
         select
            accept Deliver(Assembly: in Assembly_Type; Number: out Integer) do
               --loop
               if Can_Deliver(Assembly) then
                  Put_Line("Skompletowano " & Assembly_Name(Assembly) & " nr " &
                             Integer'Image(Assembly_Number(Assembly)));
                  for W in Product_Type loop
                     Storage(W) := Storage(W) - Assembly_Content(Assembly, W);
                     In_Storage := In_Storage - Assembly_Content(Assembly, W);
                  end loop;
                  Number := Assembly_Number(Assembly);
                  Assembly_Number(Assembly) := Assembly_Number(Assembly) + 1;
               else
                  Put_Line("Zabraklo produktow do skompletowania " & Assembly_Name(Assembly));
                  Number := 0;
               end if;
               --exit when Number /= 0;
               --delay 0.5;
               --end loop;
            end Deliver;
            Storage_Contents;

            --Storage_Contents;
         or delay Duration(2.0);
            Put_Line("Sprzedawca: Brak klientow wiec przyjmuje produkty");

            accept Take(Product: in Product_Type; Number: in Integer) do
               if Can_Accept(Product) then
                  Put_Line("Doniesiono " & Product_Name(Product) & " nr " &
                             Integer'Image(Number));
                  Storage(Product) := Storage(Product) + 1;
                  In_Storage := In_Storage + 1;
               else
                  Remove_Excess;
                  Put_Line("Na zwolnione miejsce wstawiono " & Product_Name(Product) & " nr " &
                             Integer'Image(Number));
                  Storage(Product) := Storage(Product) + 1;
                  In_Storage := In_Storage + 1;

               end if;
            end Take;
         end select;

      end loop;
   end Buffer;

begin
   for I in 1 .. Number_Of_Products loop
      P(I).Start(I, 10);
   end loop;
   for J in 1 .. Number_Of_Consumers loop
      K(J).Start(J,12);
   end loop;
end Simulation;

