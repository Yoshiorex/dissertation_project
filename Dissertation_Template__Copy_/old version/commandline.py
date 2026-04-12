--- diff for tr-task-mgr-model-16.c ---
--- /home/a/Documents/Python_gen/tr-task-mgr-model-16.c
+++ /home/a/Downloads/rtems/RTEMS-SMP-Formal-main/formal/promela/models/task-mgr/gen/tr-task-mgr-model-16.c
@@ -177,10 +177,10 @@
   
   T_log(T_NORMAL,"@@@ 4 CALL WaitForSuspend 2 resumeRC");
   T_log( T_NORMAL, "Waiting for Task(%d) to Suspend", 2, resumeRC );
-  do {
+  do {{
     resumeRC = ( *ctx->t_isSuspend )( 2 ? taskID[2] : 0xffffffff );
     rtems_task_wake_after( 100 );
-  } while (resumeRC != RTEMS_ALREADY_SUSPENDED);
+  }} while (resumeRC != RTEMS_ALREADY_SUSPENDED);
   
   /* SWITCH[2] Suspension of proc4 in favour of proc5 */
   /* SWITCH[3] ReleaseTestSyncSema of proc4 (sometime) after proc5 */
@@ -370,6 +370,7 @@
 }