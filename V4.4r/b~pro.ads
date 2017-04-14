pragma Ada_95;
with System;
package ada_main is
   pragma Warnings (Off);

   gnat_argc : Integer;
   gnat_argv : System.Address;
   gnat_envp : System.Address;

   pragma Import (C, gnat_argc);
   pragma Import (C, gnat_argv);
   pragma Import (C, gnat_envp);

   gnat_exit_status : Integer;
   pragma Import (C, gnat_exit_status);

   GNAT_Version : constant String :=
                    "GNAT Version: GPL 2015 (20150428-49)" & ASCII.NUL;
   pragma Export (C, GNAT_Version, "__gnat_version");

   Ada_Main_Program_Name : constant String := "_ada_pro" & ASCII.NUL;
   pragma Export (C, Ada_Main_Program_Name, "__gnat_ada_main_program_name");

   procedure adainit;
   pragma Export (C, adainit, "adainit");

   procedure adafinal;
   pragma Export (C, adafinal, "adafinal");

   function main
     (argc : Integer;
      argv : System.Address;
      envp : System.Address)
      return Integer;
   pragma Export (C, main, "main");

   type Version_32 is mod 2 ** 32;
   u00001 : constant Version_32 := 16#3af56ad1#;
   pragma Export (C, u00001, "proB");
   u00002 : constant Version_32 := 16#fbff4c67#;
   pragma Export (C, u00002, "system__standard_libraryB");
   u00003 : constant Version_32 := 16#13e2b8e6#;
   pragma Export (C, u00003, "system__standard_libraryS");
   u00004 : constant Version_32 := 16#3ffc8e18#;
   pragma Export (C, u00004, "adaS");
   u00005 : constant Version_32 := 16#2130723f#;
   pragma Export (C, u00005, "ada__exceptionsB");
   u00006 : constant Version_32 := 16#f9d4899e#;
   pragma Export (C, u00006, "ada__exceptionsS");
   u00007 : constant Version_32 := 16#a46739c0#;
   pragma Export (C, u00007, "ada__exceptions__last_chance_handlerB");
   u00008 : constant Version_32 := 16#3aac8c92#;
   pragma Export (C, u00008, "ada__exceptions__last_chance_handlerS");
   u00009 : constant Version_32 := 16#100301f7#;
   pragma Export (C, u00009, "systemS");
   u00010 : constant Version_32 := 16#a207fefe#;
   pragma Export (C, u00010, "system__soft_linksB");
   u00011 : constant Version_32 := 16#4b59d020#;
   pragma Export (C, u00011, "system__soft_linksS");
   u00012 : constant Version_32 := 16#b01dad17#;
   pragma Export (C, u00012, "system__parametersB");
   u00013 : constant Version_32 := 16#6e290c88#;
   pragma Export (C, u00013, "system__parametersS");
   u00014 : constant Version_32 := 16#b19b6653#;
   pragma Export (C, u00014, "system__secondary_stackB");
   u00015 : constant Version_32 := 16#bb62ce9e#;
   pragma Export (C, u00015, "system__secondary_stackS");
   u00016 : constant Version_32 := 16#39a03df9#;
   pragma Export (C, u00016, "system__storage_elementsB");
   u00017 : constant Version_32 := 16#3dc04bf3#;
   pragma Export (C, u00017, "system__storage_elementsS");
   u00018 : constant Version_32 := 16#41837d1e#;
   pragma Export (C, u00018, "system__stack_checkingB");
   u00019 : constant Version_32 := 16#9ebc6a1f#;
   pragma Export (C, u00019, "system__stack_checkingS");
   u00020 : constant Version_32 := 16#393398c1#;
   pragma Export (C, u00020, "system__exception_tableB");
   u00021 : constant Version_32 := 16#be1a67e2#;
   pragma Export (C, u00021, "system__exception_tableS");
   u00022 : constant Version_32 := 16#ce4af020#;
   pragma Export (C, u00022, "system__exceptionsB");
   u00023 : constant Version_32 := 16#78606c01#;
   pragma Export (C, u00023, "system__exceptionsS");
   u00024 : constant Version_32 := 16#37d758f1#;
   pragma Export (C, u00024, "system__exceptions__machineS");
   u00025 : constant Version_32 := 16#b895431d#;
   pragma Export (C, u00025, "system__exceptions_debugB");
   u00026 : constant Version_32 := 16#a3e11849#;
   pragma Export (C, u00026, "system__exceptions_debugS");
   u00027 : constant Version_32 := 16#570325c8#;
   pragma Export (C, u00027, "system__img_intB");
   u00028 : constant Version_32 := 16#12d8e135#;
   pragma Export (C, u00028, "system__img_intS");
   u00029 : constant Version_32 := 16#b98c3e16#;
   pragma Export (C, u00029, "system__tracebackB");
   u00030 : constant Version_32 := 16#8e3ed82c#;
   pragma Export (C, u00030, "system__tracebackS");
   u00031 : constant Version_32 := 16#9ed49525#;
   pragma Export (C, u00031, "system__traceback_entriesB");
   u00032 : constant Version_32 := 16#1058f787#;
   pragma Export (C, u00032, "system__traceback_entriesS");
   u00033 : constant Version_32 := 16#8c33a517#;
   pragma Export (C, u00033, "system__wch_conB");
   u00034 : constant Version_32 := 16#0b7e2325#;
   pragma Export (C, u00034, "system__wch_conS");
   u00035 : constant Version_32 := 16#9721e840#;
   pragma Export (C, u00035, "system__wch_stwB");
   u00036 : constant Version_32 := 16#266f0f24#;
   pragma Export (C, u00036, "system__wch_stwS");
   u00037 : constant Version_32 := 16#92b797cb#;
   pragma Export (C, u00037, "system__wch_cnvB");
   u00038 : constant Version_32 := 16#04c999d6#;
   pragma Export (C, u00038, "system__wch_cnvS");
   u00039 : constant Version_32 := 16#6033a23f#;
   pragma Export (C, u00039, "interfacesS");
   u00040 : constant Version_32 := 16#ece6fdb6#;
   pragma Export (C, u00040, "system__wch_jisB");
   u00041 : constant Version_32 := 16#84b980f7#;
   pragma Export (C, u00041, "system__wch_jisS");
   u00042 : constant Version_32 := 16#12c8cd7d#;
   pragma Export (C, u00042, "ada__tagsB");
   u00043 : constant Version_32 := 16#ce72c228#;
   pragma Export (C, u00043, "ada__tagsS");
   u00044 : constant Version_32 := 16#c3335bfd#;
   pragma Export (C, u00044, "system__htableB");
   u00045 : constant Version_32 := 16#94c1b21d#;
   pragma Export (C, u00045, "system__htableS");
   u00046 : constant Version_32 := 16#089f5cd0#;
   pragma Export (C, u00046, "system__string_hashB");
   u00047 : constant Version_32 := 16#369fd963#;
   pragma Export (C, u00047, "system__string_hashS");
   u00048 : constant Version_32 := 16#8d5ba537#;
   pragma Export (C, u00048, "system__unsigned_typesS");
   u00049 : constant Version_32 := 16#06052bd0#;
   pragma Export (C, u00049, "system__val_lluB");
   u00050 : constant Version_32 := 16#f7a9f245#;
   pragma Export (C, u00050, "system__val_lluS");
   u00051 : constant Version_32 := 16#27b600b2#;
   pragma Export (C, u00051, "system__val_utilB");
   u00052 : constant Version_32 := 16#bca3b709#;
   pragma Export (C, u00052, "system__val_utilS");
   u00053 : constant Version_32 := 16#d1060688#;
   pragma Export (C, u00053, "system__case_utilB");
   u00054 : constant Version_32 := 16#340a6820#;
   pragma Export (C, u00054, "system__case_utilS");
   u00055 : constant Version_32 := 16#28f088c2#;
   pragma Export (C, u00055, "ada__text_ioB");
   u00056 : constant Version_32 := 16#fe568dda#;
   pragma Export (C, u00056, "ada__text_ioS");
   u00057 : constant Version_32 := 16#10558b11#;
   pragma Export (C, u00057, "ada__streamsB");
   u00058 : constant Version_32 := 16#2e6701ab#;
   pragma Export (C, u00058, "ada__streamsS");
   u00059 : constant Version_32 := 16#db5c917c#;
   pragma Export (C, u00059, "ada__io_exceptionsS");
   u00060 : constant Version_32 := 16#84a27f0d#;
   pragma Export (C, u00060, "interfaces__c_streamsB");
   u00061 : constant Version_32 := 16#8bb5f2c0#;
   pragma Export (C, u00061, "interfaces__c_streamsS");
   u00062 : constant Version_32 := 16#6092d7f9#;
   pragma Export (C, u00062, "system__crtlS");
   u00063 : constant Version_32 := 16#431faf3c#;
   pragma Export (C, u00063, "system__file_ioB");
   u00064 : constant Version_32 := 16#b772e092#;
   pragma Export (C, u00064, "system__file_ioS");
   u00065 : constant Version_32 := 16#b7ab275c#;
   pragma Export (C, u00065, "ada__finalizationB");
   u00066 : constant Version_32 := 16#19f764ca#;
   pragma Export (C, u00066, "ada__finalizationS");
   u00067 : constant Version_32 := 16#95817ed8#;
   pragma Export (C, u00067, "system__finalization_rootB");
   u00068 : constant Version_32 := 16#5ff17267#;
   pragma Export (C, u00068, "system__finalization_rootS");
   u00069 : constant Version_32 := 16#769e25e6#;
   pragma Export (C, u00069, "interfaces__cB");
   u00070 : constant Version_32 := 16#4a38bedb#;
   pragma Export (C, u00070, "interfaces__cS");
   u00071 : constant Version_32 := 16#0ac2ab10#;
   pragma Export (C, u00071, "system__os_libB");
   u00072 : constant Version_32 := 16#d7b69782#;
   pragma Export (C, u00072, "system__os_libS");
   u00073 : constant Version_32 := 16#1a817b8e#;
   pragma Export (C, u00073, "system__stringsB");
   u00074 : constant Version_32 := 16#6ebc1091#;
   pragma Export (C, u00074, "system__stringsS");
   u00075 : constant Version_32 := 16#ed9c9b5f#;
   pragma Export (C, u00075, "system__file_control_blockS");
   u00076 : constant Version_32 := 16#fd83e873#;
   pragma Export (C, u00076, "system__concat_2B");
   u00077 : constant Version_32 := 16#12a3d627#;
   pragma Export (C, u00077, "system__concat_2S");
   u00078 : constant Version_32 := 16#91b1145a#;
   pragma Export (C, u00078, "system__taskingB");
   u00079 : constant Version_32 := 16#0614561b#;
   pragma Export (C, u00079, "system__taskingS");
   u00080 : constant Version_32 := 16#4ef49049#;
   pragma Export (C, u00080, "system__task_primitivesS");
   u00081 : constant Version_32 := 16#b498031e#;
   pragma Export (C, u00081, "system__os_interfaceB");
   u00082 : constant Version_32 := 16#fe105d7f#;
   pragma Export (C, u00082, "system__os_interfaceS");
   u00083 : constant Version_32 := 16#f71cb626#;
   pragma Export (C, u00083, "interfaces__c__extensionsS");
   u00084 : constant Version_32 := 16#434c88e8#;
   pragma Export (C, u00084, "system__os_constantsS");
   u00085 : constant Version_32 := 16#c920fb2e#;
   pragma Export (C, u00085, "system__task_primitives__operationsB");
   u00086 : constant Version_32 := 16#0e983487#;
   pragma Export (C, u00086, "system__task_primitives__operationsS");
   u00087 : constant Version_32 := 16#89b55e64#;
   pragma Export (C, u00087, "system__interrupt_managementB");
   u00088 : constant Version_32 := 16#bec4dbfd#;
   pragma Export (C, u00088, "system__interrupt_managementS");
   u00089 : constant Version_32 := 16#f65595cf#;
   pragma Export (C, u00089, "system__multiprocessorsB");
   u00090 : constant Version_32 := 16#28af9e84#;
   pragma Export (C, u00090, "system__multiprocessorsS");
   u00091 : constant Version_32 := 16#820eb304#;
   pragma Export (C, u00091, "system__os_primitivesB");
   u00092 : constant Version_32 := 16#a0d28dde#;
   pragma Export (C, u00092, "system__os_primitivesS");
   u00093 : constant Version_32 := 16#e0fce7f8#;
   pragma Export (C, u00093, "system__task_infoB");
   u00094 : constant Version_32 := 16#9019bcf6#;
   pragma Export (C, u00094, "system__task_infoS");
   u00095 : constant Version_32 := 16#85267e7e#;
   pragma Export (C, u00095, "system__tasking__debugB");
   u00096 : constant Version_32 := 16#62f5b061#;
   pragma Export (C, u00096, "system__tasking__debugS");
   u00097 : constant Version_32 := 16#2b70b149#;
   pragma Export (C, u00097, "system__concat_3B");
   u00098 : constant Version_32 := 16#1b735d52#;
   pragma Export (C, u00098, "system__concat_3S");
   u00099 : constant Version_32 := 16#d0432c8d#;
   pragma Export (C, u00099, "system__img_enum_newB");
   u00100 : constant Version_32 := 16#714f0737#;
   pragma Export (C, u00100, "system__img_enum_newS");
   u00101 : constant Version_32 := 16#9777733a#;
   pragma Export (C, u00101, "system__img_lliB");
   u00102 : constant Version_32 := 16#014c5426#;
   pragma Export (C, u00102, "system__img_lliS");
   u00103 : constant Version_32 := 16#118e865d#;
   pragma Export (C, u00103, "system__stack_usageB");
   u00104 : constant Version_32 := 16#00bc3311#;
   pragma Export (C, u00104, "system__stack_usageS");
   u00105 : constant Version_32 := 16#d7aac20c#;
   pragma Export (C, u00105, "system__ioB");
   u00106 : constant Version_32 := 16#8e41f6b8#;
   pragma Export (C, u00106, "system__ioS");
   u00107 : constant Version_32 := 16#f83990e5#;
   pragma Export (C, u00107, "system__tasking__protected_objectsB");
   u00108 : constant Version_32 := 16#63b50013#;
   pragma Export (C, u00108, "system__tasking__protected_objectsS");
   u00109 : constant Version_32 := 16#a115ede1#;
   pragma Export (C, u00109, "system__soft_links__taskingB");
   u00110 : constant Version_32 := 16#e47ef8be#;
   pragma Export (C, u00110, "system__soft_links__taskingS");
   u00111 : constant Version_32 := 16#17d21067#;
   pragma Export (C, u00111, "ada__exceptions__is_null_occurrenceB");
   u00112 : constant Version_32 := 16#9a9e8fd3#;
   pragma Export (C, u00112, "ada__exceptions__is_null_occurrenceS");
   u00113 : constant Version_32 := 16#ee80728a#;
   pragma Export (C, u00113, "system__tracesB");
   u00114 : constant Version_32 := 16#e21e695d#;
   pragma Export (C, u00114, "system__tracesS");
   u00115 : constant Version_32 := 16#9d39c675#;
   pragma Export (C, u00115, "system__memoryB");
   u00116 : constant Version_32 := 16#497e67c3#;
   pragma Export (C, u00116, "system__memoryS");
   --  BEGIN ELABORATION ORDER
   --  ada%s
   --  interfaces%s
   --  system%s
   --  system.case_util%s
   --  system.case_util%b
   --  system.htable%s
   --  system.img_enum_new%s
   --  system.img_enum_new%b
   --  system.img_int%s
   --  system.img_int%b
   --  system.img_lli%s
   --  system.img_lli%b
   --  system.io%s
   --  system.io%b
   --  system.multiprocessors%s
   --  system.os_primitives%s
   --  system.os_primitives%b
   --  system.parameters%s
   --  system.parameters%b
   --  system.crtl%s
   --  interfaces.c_streams%s
   --  interfaces.c_streams%b
   --  system.standard_library%s
   --  system.exceptions_debug%s
   --  system.exceptions_debug%b
   --  system.storage_elements%s
   --  system.storage_elements%b
   --  system.stack_checking%s
   --  system.stack_checking%b
   --  system.stack_usage%s
   --  system.stack_usage%b
   --  system.string_hash%s
   --  system.string_hash%b
   --  system.htable%b
   --  system.strings%s
   --  system.strings%b
   --  system.os_lib%s
   --  system.task_info%s
   --  system.task_info%b
   --  system.traceback_entries%s
   --  system.traceback_entries%b
   --  ada.exceptions%s
   --  ada.exceptions.is_null_occurrence%s
   --  ada.exceptions.is_null_occurrence%b
   --  system.soft_links%s
   --  system.traces%s
   --  system.traces%b
   --  system.unsigned_types%s
   --  system.val_llu%s
   --  system.val_util%s
   --  system.val_util%b
   --  system.val_llu%b
   --  system.wch_con%s
   --  system.wch_con%b
   --  system.wch_cnv%s
   --  system.wch_jis%s
   --  system.wch_jis%b
   --  system.wch_cnv%b
   --  system.wch_stw%s
   --  system.wch_stw%b
   --  ada.exceptions.last_chance_handler%s
   --  ada.exceptions.last_chance_handler%b
   --  system.concat_2%s
   --  system.concat_2%b
   --  system.concat_3%s
   --  system.concat_3%b
   --  system.exception_table%s
   --  system.exception_table%b
   --  ada.io_exceptions%s
   --  ada.tags%s
   --  ada.streams%s
   --  ada.streams%b
   --  interfaces.c%s
   --  system.multiprocessors%b
   --  interfaces.c.extensions%s
   --  system.exceptions%s
   --  system.exceptions%b
   --  system.exceptions.machine%s
   --  system.file_control_block%s
   --  system.file_io%s
   --  system.finalization_root%s
   --  system.finalization_root%b
   --  ada.finalization%s
   --  ada.finalization%b
   --  system.os_constants%s
   --  system.os_interface%s
   --  system.os_interface%b
   --  system.interrupt_management%s
   --  system.interrupt_management%b
   --  system.task_primitives%s
   --  system.tasking%s
   --  system.task_primitives.operations%s
   --  system.tasking%b
   --  system.tasking.debug%s
   --  system.tasking.debug%b
   --  system.task_primitives.operations%b
   --  system.memory%s
   --  system.memory%b
   --  system.standard_library%b
   --  system.secondary_stack%s
   --  system.file_io%b
   --  interfaces.c%b
   --  ada.tags%b
   --  system.soft_links%b
   --  system.os_lib%b
   --  system.secondary_stack%b
   --  system.soft_links.tasking%s
   --  system.soft_links.tasking%b
   --  system.traceback%s
   --  ada.exceptions%b
   --  system.traceback%b
   --  ada.text_io%s
   --  ada.text_io%b
   --  system.tasking.protected_objects%s
   --  system.tasking.protected_objects%b
   --  pro%b
   --  END ELABORATION ORDER


end ada_main;
