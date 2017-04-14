pragma Ada_95;
pragma Warnings (Off);
pragma Source_File_Name (ada_main, Spec_File_Name => "b~mc2aut.ads");
pragma Source_File_Name (ada_main, Body_File_Name => "b~mc2aut.adb");
pragma Suppress (Overflow_Check);

with System.Restrictions;
with Ada.Exceptions;

package body ada_main is

   E147 : Short_Integer; pragma Import (Ada, E147, "system__os_lib_E");
   E013 : Short_Integer; pragma Import (Ada, E013, "system__soft_links_E");
   E023 : Short_Integer; pragma Import (Ada, E023, "system__exception_table_E");
   E075 : Short_Integer; pragma Import (Ada, E075, "ada__containers_E");
   E128 : Short_Integer; pragma Import (Ada, E128, "ada__io_exceptions_E");
   E066 : Short_Integer; pragma Import (Ada, E066, "ada__strings_E");
   E068 : Short_Integer; pragma Import (Ada, E068, "ada__strings__maps_E");
   E072 : Short_Integer; pragma Import (Ada, E072, "ada__strings__maps__constants_E");
   E106 : Short_Integer; pragma Import (Ada, E106, "ada__tags_E");
   E127 : Short_Integer; pragma Import (Ada, E127, "ada__streams_E");
   E057 : Short_Integer; pragma Import (Ada, E057, "interfaces__c_E");
   E025 : Short_Integer; pragma Import (Ada, E025, "system__exceptions_E");
   E150 : Short_Integer; pragma Import (Ada, E150, "system__file_control_block_E");
   E143 : Short_Integer; pragma Import (Ada, E143, "system__file_io_E");
   E130 : Short_Integer; pragma Import (Ada, E130, "system__finalization_root_E");
   E125 : Short_Integer; pragma Import (Ada, E125, "ada__finalization_E");
   E132 : Short_Integer; pragma Import (Ada, E132, "system__storage_pools_E");
   E120 : Short_Integer; pragma Import (Ada, E120, "system__finalization_masters_E");
   E118 : Short_Integer; pragma Import (Ada, E118, "system__storage_pools__subpools_E");
   E055 : Short_Integer; pragma Import (Ada, E055, "ada__calendar_E");
   E053 : Short_Integer; pragma Import (Ada, E053, "ada__calendar__delays_E");
   E081 : Short_Integer; pragma Import (Ada, E081, "ada__calendar__time_zones_E");
   E200 : Short_Integer; pragma Import (Ada, E200, "gnat__directory_operations_E");
   E051 : Short_Integer; pragma Import (Ada, E051, "system__assertions_E");
   E206 : Short_Integer; pragma Import (Ada, E206, "system__direct_io_E");
   E208 : Short_Integer; pragma Import (Ada, E208, "system__pool_global_E");
   E017 : Short_Integer; pragma Import (Ada, E017, "system__secondary_stack_E");
   E104 : Short_Integer; pragma Import (Ada, E104, "ada__strings__unbounded_E");
   E077 : Short_Integer; pragma Import (Ada, E077, "ada__directories_E");
   E152 : Short_Integer; pragma Import (Ada, E152, "system__regexp_E");
   E216 : Short_Integer; pragma Import (Ada, E216, "system__tasking__initialization_E");
   E154 : Short_Integer; pragma Import (Ada, E154, "ada__real_time_E");
   E184 : Short_Integer; pragma Import (Ada, E184, "ada__text_io_E");
   E226 : Short_Integer; pragma Import (Ada, E226, "system__tasking__protected_objects_E");
   E228 : Short_Integer; pragma Import (Ada, E228, "system__tasking__protected_objects__entries_E");
   E224 : Short_Integer; pragma Import (Ada, E224, "system__tasking__queuing_E");
   E212 : Short_Integer; pragma Import (Ada, E212, "system__tasking__stages_E");
   E186 : Short_Integer; pragma Import (Ada, E186, "bool_dyn_store_E");
   E190 : Short_Integer; pragma Import (Ada, E190, "dyn_store_E");
   E191 : Short_Integer; pragma Import (Ada, E191, "flags_E");
   E188 : Short_Integer; pragma Import (Ada, E188, "configurations_E");
   E193 : Short_Integer; pragma Import (Ada, E193, "my_atomic_counters_E");
   E203 : Short_Integer; pragma Import (Ada, E203, "nicknames_E");

   Local_Priority_Specific_Dispatching : constant String := "";
   Local_Interrupt_States : constant String := "";

   Is_Elaborated : Boolean := False;

   procedure finalize_library is
   begin
      E228 := E228 - 1;
      declare
         procedure F1;
         pragma Import (Ada, F1, "system__tasking__protected_objects__entries__finalize_spec");
      begin
         F1;
      end;
      E184 := E184 - 1;
      declare
         procedure F2;
         pragma Import (Ada, F2, "ada__text_io__finalize_spec");
      begin
         F2;
      end;
      E077 := E077 - 1;
      E152 := E152 - 1;
      declare
         procedure F3;
         pragma Import (Ada, F3, "system__regexp__finalize_spec");
      begin
         F3;
      end;
      declare
         procedure F4;
         pragma Import (Ada, F4, "ada__directories__finalize_spec");
      begin
         F4;
      end;
      E104 := E104 - 1;
      declare
         procedure F5;
         pragma Import (Ada, F5, "ada__strings__unbounded__finalize_spec");
      begin
         F5;
      end;
      declare
         procedure F6;
         pragma Import (Ada, F6, "system__file_io__finalize_body");
      begin
         E143 := E143 - 1;
         F6;
      end;
      E120 := E120 - 1;
      E118 := E118 - 1;
      E208 := E208 - 1;
      declare
         procedure F7;
         pragma Import (Ada, F7, "system__pool_global__finalize_spec");
      begin
         F7;
      end;
      E206 := E206 - 1;
      declare
         procedure F8;
         pragma Import (Ada, F8, "system__direct_io__finalize_spec");
      begin
         F8;
      end;
      declare
         procedure F9;
         pragma Import (Ada, F9, "system__storage_pools__subpools__finalize_spec");
      begin
         F9;
      end;
      declare
         procedure F10;
         pragma Import (Ada, F10, "system__finalization_masters__finalize_spec");
      begin
         F10;
      end;
      declare
         procedure Reraise_Library_Exception_If_Any;
            pragma Import (Ada, Reraise_Library_Exception_If_Any, "__gnat_reraise_library_exception_if_any");
      begin
         Reraise_Library_Exception_If_Any;
      end;
   end finalize_library;

   procedure adafinal is
      procedure s_stalib_adafinal;
      pragma Import (C, s_stalib_adafinal, "system__standard_library__adafinal");

      procedure Runtime_Finalize;
      pragma Import (C, Runtime_Finalize, "__gnat_runtime_finalize");

   begin
      if not Is_Elaborated then
         return;
      end if;
      Is_Elaborated := False;
      Runtime_Finalize;
      s_stalib_adafinal;
   end adafinal;

   type No_Param_Proc is access procedure;

   procedure adainit is
      Main_Priority : Integer;
      pragma Import (C, Main_Priority, "__gl_main_priority");
      Time_Slice_Value : Integer;
      pragma Import (C, Time_Slice_Value, "__gl_time_slice_val");
      WC_Encoding : Character;
      pragma Import (C, WC_Encoding, "__gl_wc_encoding");
      Locking_Policy : Character;
      pragma Import (C, Locking_Policy, "__gl_locking_policy");
      Queuing_Policy : Character;
      pragma Import (C, Queuing_Policy, "__gl_queuing_policy");
      Task_Dispatching_Policy : Character;
      pragma Import (C, Task_Dispatching_Policy, "__gl_task_dispatching_policy");
      Priority_Specific_Dispatching : System.Address;
      pragma Import (C, Priority_Specific_Dispatching, "__gl_priority_specific_dispatching");
      Num_Specific_Dispatching : Integer;
      pragma Import (C, Num_Specific_Dispatching, "__gl_num_specific_dispatching");
      Main_CPU : Integer;
      pragma Import (C, Main_CPU, "__gl_main_cpu");
      Interrupt_States : System.Address;
      pragma Import (C, Interrupt_States, "__gl_interrupt_states");
      Num_Interrupt_States : Integer;
      pragma Import (C, Num_Interrupt_States, "__gl_num_interrupt_states");
      Unreserve_All_Interrupts : Integer;
      pragma Import (C, Unreserve_All_Interrupts, "__gl_unreserve_all_interrupts");
      Detect_Blocking : Integer;
      pragma Import (C, Detect_Blocking, "__gl_detect_blocking");
      Default_Stack_Size : Integer;
      pragma Import (C, Default_Stack_Size, "__gl_default_stack_size");
      Leap_Seconds_Support : Integer;
      pragma Import (C, Leap_Seconds_Support, "__gl_leap_seconds_support");
      Bind_Env_Addr : System.Address;
      pragma Import (C, Bind_Env_Addr, "__gl_bind_env_addr");

      procedure Runtime_Initialize (Install_Handler : Integer);
      pragma Import (C, Runtime_Initialize, "__gnat_runtime_initialize");

      Finalize_Library_Objects : No_Param_Proc;
      pragma Import (C, Finalize_Library_Objects, "__gnat_finalize_library_objects");
   begin
      if Is_Elaborated then
         return;
      end if;
      Is_Elaborated := True;
      Main_Priority := -1;
      Time_Slice_Value := -1;
      WC_Encoding := 'b';
      Locking_Policy := ' ';
      Queuing_Policy := ' ';
      Task_Dispatching_Policy := ' ';
      System.Restrictions.Run_Time_Restrictions :=
        (Set =>
          (False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, True, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False),
         Value => (0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
         Violated =>
          (False, False, False, True, True, True, False, True, 
           False, False, True, True, True, True, False, False, 
           True, False, False, True, True, False, True, True, 
           False, True, True, True, True, False, False, False, 
           False, False, True, False, False, True, False, True, 
           False, False, True, False, True, False, True, False, 
           False, True, True, False, False, True, False, False, 
           True, False, True, False, True, True, True, False, 
           False, True, False, True, True, True, False, True, 
           True, False, True, True, True, True, False, False, 
           True, False, False, False, False, False, False, True, 
           False, False, False),
         Count => (0, 0, 0, 0, 0, 0, 1, 0, 0, 0),
         Unknown => (False, False, False, False, False, False, True, False, False, False));
      Priority_Specific_Dispatching :=
        Local_Priority_Specific_Dispatching'Address;
      Num_Specific_Dispatching := 0;
      Main_CPU := -1;
      Interrupt_States := Local_Interrupt_States'Address;
      Num_Interrupt_States := 0;
      Unreserve_All_Interrupts := 0;
      Detect_Blocking := 0;
      Default_Stack_Size := -1;
      Leap_Seconds_Support := 0;

      Runtime_Initialize (1);

      Finalize_Library_Objects := finalize_library'access;

      System.Soft_Links'Elab_Spec;
      System.Exception_Table'Elab_Body;
      E023 := E023 + 1;
      Ada.Containers'Elab_Spec;
      E075 := E075 + 1;
      Ada.Io_Exceptions'Elab_Spec;
      E128 := E128 + 1;
      Ada.Strings'Elab_Spec;
      E066 := E066 + 1;
      Ada.Strings.Maps'Elab_Spec;
      Ada.Strings.Maps.Constants'Elab_Spec;
      E072 := E072 + 1;
      Ada.Tags'Elab_Spec;
      Ada.Streams'Elab_Spec;
      E127 := E127 + 1;
      Interfaces.C'Elab_Spec;
      System.Exceptions'Elab_Spec;
      E025 := E025 + 1;
      System.File_Control_Block'Elab_Spec;
      E150 := E150 + 1;
      System.Finalization_Root'Elab_Spec;
      E130 := E130 + 1;
      Ada.Finalization'Elab_Spec;
      E125 := E125 + 1;
      System.Storage_Pools'Elab_Spec;
      E132 := E132 + 1;
      System.Finalization_Masters'Elab_Spec;
      System.Storage_Pools.Subpools'Elab_Spec;
      Ada.Calendar'Elab_Spec;
      Ada.Calendar'Elab_Body;
      E055 := E055 + 1;
      Ada.Calendar.Delays'Elab_Body;
      E053 := E053 + 1;
      Ada.Calendar.Time_Zones'Elab_Spec;
      E081 := E081 + 1;
      Gnat.Directory_Operations'Elab_Spec;
      System.Assertions'Elab_Spec;
      E051 := E051 + 1;
      System.Direct_Io'Elab_Spec;
      E206 := E206 + 1;
      System.Pool_Global'Elab_Spec;
      E208 := E208 + 1;
      E118 := E118 + 1;
      System.Finalization_Masters'Elab_Body;
      E120 := E120 + 1;
      System.File_Io'Elab_Body;
      E143 := E143 + 1;
      E057 := E057 + 1;
      Ada.Tags'Elab_Body;
      E106 := E106 + 1;
      E068 := E068 + 1;
      System.Soft_Links'Elab_Body;
      E013 := E013 + 1;
      System.Os_Lib'Elab_Body;
      E147 := E147 + 1;
      System.Secondary_Stack'Elab_Body;
      E017 := E017 + 1;
      Gnat.Directory_Operations'Elab_Body;
      E200 := E200 + 1;
      Ada.Strings.Unbounded'Elab_Spec;
      E104 := E104 + 1;
      Ada.Directories'Elab_Spec;
      System.Regexp'Elab_Spec;
      E152 := E152 + 1;
      Ada.Directories'Elab_Body;
      E077 := E077 + 1;
      System.Tasking.Initialization'Elab_Body;
      E216 := E216 + 1;
      Ada.Real_Time'Elab_Spec;
      Ada.Real_Time'Elab_Body;
      E154 := E154 + 1;
      Ada.Text_Io'Elab_Spec;
      Ada.Text_Io'Elab_Body;
      E184 := E184 + 1;
      System.Tasking.Protected_Objects'Elab_Body;
      E226 := E226 + 1;
      System.Tasking.Protected_Objects.Entries'Elab_Spec;
      E228 := E228 + 1;
      System.Tasking.Queuing'Elab_Body;
      E224 := E224 + 1;
      System.Tasking.Stages'Elab_Body;
      E212 := E212 + 1;
      E186 := E186 + 1;
      FLAGS'ELAB_SPEC;
      E191 := E191 + 1;
      E193 := E193 + 1;
      E190 := E190 + 1;
      E203 := E203 + 1;
      E188 := E188 + 1;
   end adainit;

   procedure Ada_Main_Program;
   pragma Import (Ada, Ada_Main_Program, "_ada_mc2aut");

   function main
     (argc : Integer;
      argv : System.Address;
      envp : System.Address)
      return Integer
   is
      procedure Initialize (Addr : System.Address);
      pragma Import (C, Initialize, "__gnat_initialize");

      procedure Finalize;
      pragma Import (C, Finalize, "__gnat_finalize");
      SEH : aliased array (1 .. 2) of Integer;

      Ensure_Reference : aliased System.Address := Ada_Main_Program_Name'Address;
      pragma Volatile (Ensure_Reference);

   begin
      gnat_argc := argc;
      gnat_argv := argv;
      gnat_envp := envp;

      Initialize (SEH'Address);
      adainit;
      Ada_Main_Program;
      adafinal;
      Finalize;
      return (gnat_exit_status);
   end;

--  BEGIN Object file/option list
   --   ./spin_locks.o
   --   ./bool_dyn_store.o
   --   ./flags.o
   --   ./my_atomic_counters.o
   --   ./dyn_store.o
   --   ./nicknames.o
   --   ./configurations.o
   --   ./mc2aut.o
   --   -L./
   --   -L/project/Ada/GNAT/2016/lib/gcc/x86_64-apple-darwin14.5.0/4.9.4/adalib/
   --   -static
   --   -lgnarl
   --   -lgnat
--  END Object file/option list   

end ada_main;
