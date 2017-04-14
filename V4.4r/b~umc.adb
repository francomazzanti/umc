pragma Ada_95;
pragma Warnings (Off);
pragma Source_File_Name (ada_main, Spec_File_Name => "b~umc.ads");
pragma Source_File_Name (ada_main, Body_File_Name => "b~umc.adb");
pragma Suppress (Overflow_Check);

with System.Restrictions;
with Ada.Exceptions;

package body ada_main is

   E138 : Short_Integer; pragma Import (Ada, E138, "system__os_lib_E");
   E013 : Short_Integer; pragma Import (Ada, E013, "system__soft_links_E");
   E019 : Short_Integer; pragma Import (Ada, E019, "system__exception_table_E");
   E153 : Short_Integer; pragma Import (Ada, E153, "ada__containers_E");
   E119 : Short_Integer; pragma Import (Ada, E119, "ada__io_exceptions_E");
   E082 : Short_Integer; pragma Import (Ada, E082, "ada__strings_E");
   E084 : Short_Integer; pragma Import (Ada, E084, "ada__strings__maps_E");
   E087 : Short_Integer; pragma Import (Ada, E087, "ada__strings__maps__constants_E");
   E097 : Short_Integer; pragma Import (Ada, E097, "ada__tags_E");
   E118 : Short_Integer; pragma Import (Ada, E118, "ada__streams_E");
   E055 : Short_Integer; pragma Import (Ada, E055, "interfaces__c_E");
   E021 : Short_Integer; pragma Import (Ada, E021, "system__exceptions_E");
   E141 : Short_Integer; pragma Import (Ada, E141, "system__file_control_block_E");
   E134 : Short_Integer; pragma Import (Ada, E134, "system__file_io_E");
   E121 : Short_Integer; pragma Import (Ada, E121, "system__finalization_root_E");
   E116 : Short_Integer; pragma Import (Ada, E116, "ada__finalization_E");
   E123 : Short_Integer; pragma Import (Ada, E123, "system__storage_pools_E");
   E111 : Short_Integer; pragma Import (Ada, E111, "system__finalization_masters_E");
   E109 : Short_Integer; pragma Import (Ada, E109, "system__storage_pools__subpools_E");
   E185 : Short_Integer; pragma Import (Ada, E185, "ada__synchronous_task_control_E");
   E053 : Short_Integer; pragma Import (Ada, E053, "ada__calendar_E");
   E150 : Short_Integer; pragma Import (Ada, E150, "ada__calendar__delays_E");
   E061 : Short_Integer; pragma Import (Ada, E061, "ada__calendar__time_zones_E");
   E231 : Short_Integer; pragma Import (Ada, E231, "gnat__directory_operations_E");
   E267 : Short_Integer; pragma Import (Ada, E267, "system__pool_global_E");
   E009 : Short_Integer; pragma Import (Ada, E009, "system__secondary_stack_E");
   E095 : Short_Integer; pragma Import (Ada, E095, "ada__strings__unbounded_E");
   E051 : Short_Integer; pragma Import (Ada, E051, "ada__directories_E");
   E143 : Short_Integer; pragma Import (Ada, E143, "system__regexp_E");
   E191 : Short_Integer; pragma Import (Ada, E191, "system__tasking__initialization_E");
   E155 : Short_Integer; pragma Import (Ada, E155, "ada__real_time_E");
   E145 : Short_Integer; pragma Import (Ada, E145, "ada__text_io_E");
   E255 : Short_Integer; pragma Import (Ada, E255, "gnat__io_aux_E");
   E201 : Short_Integer; pragma Import (Ada, E201, "system__tasking__protected_objects_E");
   E203 : Short_Integer; pragma Import (Ada, E203, "system__tasking__protected_objects__entries_E");
   E199 : Short_Integer; pragma Import (Ada, E199, "system__tasking__queuing_E");
   E277 : Short_Integer; pragma Import (Ada, E277, "system__tasking__stages_E");
   E239 : Short_Integer; pragma Import (Ada, E239, "dotlib_E");
   E222 : Short_Integer; pragma Import (Ada, E222, "dyn_store_E");
   E253 : Short_Integer; pragma Import (Ada, E253, "dyn_store_obj_E");
   E146 : Short_Integer; pragma Import (Ada, E146, "flags_E");
   E220 : Short_Integer; pragma Import (Ada, E220, "configurations_E");
   E237 : Short_Integer; pragma Import (Ada, E237, "dotgen_E");
   E257 : Short_Integer; pragma Import (Ada, E257, "mc_E");
   E148 : Short_Integer; pragma Import (Ada, E148, "mc_server_E");
   E224 : Short_Integer; pragma Import (Ada, E224, "my_atomic_counters_E");
   E259 : Short_Integer; pragma Import (Ada, E259, "my_hash_E");
   E234 : Short_Integer; pragma Import (Ada, E234, "nicknames_E");
   E263 : Short_Integer; pragma Import (Ada, E263, "uctl_E");

   Local_Priority_Specific_Dispatching : constant String := "";
   Local_Interrupt_States : constant String := "";

   Is_Elaborated : Boolean := False;

   procedure finalize_library is
   begin
      E203 := E203 - 1;
      declare
         procedure F1;
         pragma Import (Ada, F1, "system__tasking__protected_objects__entries__finalize_spec");
      begin
         F1;
      end;
      E145 := E145 - 1;
      declare
         procedure F2;
         pragma Import (Ada, F2, "ada__text_io__finalize_spec");
      begin
         F2;
      end;
      E051 := E051 - 1;
      E143 := E143 - 1;
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
      E095 := E095 - 1;
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
         E134 := E134 - 1;
         F6;
      end;
      E111 := E111 - 1;
      E109 := E109 - 1;
      E267 := E267 - 1;
      declare
         procedure F7;
         pragma Import (Ada, F7, "system__pool_global__finalize_spec");
      begin
         F7;
      end;
      E185 := E185 - 1;
      declare
         procedure F8;
         pragma Import (Ada, F8, "ada__synchronous_task_control__finalize_spec");
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
      Main_Priority := 31;
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
          (False, True, False, True, True, True, False, True, 
           False, False, True, True, True, True, False, False, 
           True, False, False, True, True, False, True, True, 
           False, True, True, True, True, False, False, False, 
           False, False, True, False, False, True, False, True, 
           False, False, True, False, True, False, True, True, 
           False, True, True, False, False, True, False, False, 
           True, False, True, False, True, True, True, True, 
           False, True, False, True, True, True, False, True, 
           True, False, True, True, True, True, False, False, 
           True, False, False, False, False, True, True, True, 
           False, False, False),
         Count => (0, 0, 0, 0, 22, 22, 3, 0, 0, 0),
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
      E019 := E019 + 1;
      Ada.Containers'Elab_Spec;
      E153 := E153 + 1;
      Ada.Io_Exceptions'Elab_Spec;
      E119 := E119 + 1;
      Ada.Strings'Elab_Spec;
      E082 := E082 + 1;
      Ada.Strings.Maps'Elab_Spec;
      Ada.Strings.Maps.Constants'Elab_Spec;
      E087 := E087 + 1;
      Ada.Tags'Elab_Spec;
      Ada.Streams'Elab_Spec;
      E118 := E118 + 1;
      Interfaces.C'Elab_Spec;
      System.Exceptions'Elab_Spec;
      E021 := E021 + 1;
      System.File_Control_Block'Elab_Spec;
      E141 := E141 + 1;
      System.Finalization_Root'Elab_Spec;
      E121 := E121 + 1;
      Ada.Finalization'Elab_Spec;
      E116 := E116 + 1;
      System.Storage_Pools'Elab_Spec;
      E123 := E123 + 1;
      System.Finalization_Masters'Elab_Spec;
      System.Storage_Pools.Subpools'Elab_Spec;
      Ada.Synchronous_Task_Control'Elab_Spec;
      E185 := E185 + 1;
      Ada.Calendar'Elab_Spec;
      Ada.Calendar'Elab_Body;
      E053 := E053 + 1;
      Ada.Calendar.Delays'Elab_Body;
      E150 := E150 + 1;
      Ada.Calendar.Time_Zones'Elab_Spec;
      E061 := E061 + 1;
      Gnat.Directory_Operations'Elab_Spec;
      System.Pool_Global'Elab_Spec;
      E267 := E267 + 1;
      E109 := E109 + 1;
      System.Finalization_Masters'Elab_Body;
      E111 := E111 + 1;
      System.File_Io'Elab_Body;
      E134 := E134 + 1;
      E055 := E055 + 1;
      Ada.Tags'Elab_Body;
      E097 := E097 + 1;
      E084 := E084 + 1;
      System.Soft_Links'Elab_Body;
      E013 := E013 + 1;
      System.Os_Lib'Elab_Body;
      E138 := E138 + 1;
      System.Secondary_Stack'Elab_Body;
      E009 := E009 + 1;
      Gnat.Directory_Operations'Elab_Body;
      E231 := E231 + 1;
      Ada.Strings.Unbounded'Elab_Spec;
      E095 := E095 + 1;
      Ada.Directories'Elab_Spec;
      System.Regexp'Elab_Spec;
      E143 := E143 + 1;
      Ada.Directories'Elab_Body;
      E051 := E051 + 1;
      System.Tasking.Initialization'Elab_Body;
      E191 := E191 + 1;
      Ada.Real_Time'Elab_Spec;
      Ada.Real_Time'Elab_Body;
      E155 := E155 + 1;
      Ada.Text_Io'Elab_Spec;
      Ada.Text_Io'Elab_Body;
      E145 := E145 + 1;
      E255 := E255 + 1;
      System.Tasking.Protected_Objects'Elab_Body;
      E201 := E201 + 1;
      System.Tasking.Protected_Objects.Entries'Elab_Spec;
      E203 := E203 + 1;
      System.Tasking.Queuing'Elab_Body;
      E199 := E199 + 1;
      System.Tasking.Stages'Elab_Body;
      E277 := E277 + 1;
      FLAGS'ELAB_SPEC;
      E146 := E146 + 1;
      E239 := E239 + 1;
      E237 := E237 + 1;
      E224 := E224 + 1;
      E253 := E253 + 1;
      E222 := E222 + 1;
      E259 := E259 + 1;
      E234 := E234 + 1;
      E220 := E220 + 1;
      E263 := E263 + 1;
      Mc_Server'Elab_Body;
      E148 := E148 + 1;
      E257 := E257 + 1;
   end adainit;

   procedure Ada_Main_Program;
   pragma Import (Ada, Ada_Main_Program, "_ada_umc");

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
   --   ./flags.o
   --   ./dotlib.o
   --   ./dotgen.o
   --   ./umc.o
   --   ./my_atomic_counters.o
   --   ./dyn_store_obj.o
   --   ./dyn_store.o
   --   ./my_hash.o
   --   ./nicknames.o
   --   ./configurations.o
   --   ./uctl.o
   --   ./mc_server.o
   --   ./mc.o
   --   -L./
   --   -L/project/Ada/GNAT/2016/lib/gcc/x86_64-apple-darwin14.5.0/4.9.4/adalib/
   --   -static
   --   -lgnarl
   --   -lgnat
--  END Object file/option list   

end ada_main;
