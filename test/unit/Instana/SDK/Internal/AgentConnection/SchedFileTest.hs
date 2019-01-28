{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Instana.SDK.Internal.AgentConnection.SchedFileTest (allTests) where


import           Test.HUnit

import           Instana.SDK.Internal.AgentConnection.SchedFile (parsePidFromSchedFile)


allTests :: Test
allTests =
  TestList
    [ TestLabel "shouldParsePidFromSchedFile" shouldParsePidFromSchedFile
    , TestLabel "shouldReturnNothingIfNoMatch" shouldReturnNothingIfNoMatch
    , TestLabel "shouldReturnNothingIfEmptyFile" shouldReturnNothingIfEmptyFile
    ]


shouldParsePidFromSchedFile :: Test
shouldParsePidFromSchedFile =
  let
    parsedPid = parsePidFromSchedFile schedFileContent
  in
  TestCase $
    assertEqual "parsed PID" (Just "22651") parsedPid


shouldReturnNothingIfNoMatch :: Test
shouldReturnNothingIfNoMatch =
  let
    parsedPid = parsePidFromSchedFile schedFileNoMatch
  in
  TestCase $
    assertEqual "parsed PID" Nothing parsedPid


shouldReturnNothingIfEmptyFile :: Test
shouldReturnNothingIfEmptyFile =
  let
    parsedPid = parsePidFromSchedFile ""
  in
  TestCase $
    assertEqual "parsed PID" Nothing parsedPid


schedFileContent :: String
schedFileContent =
  "apache2 (22651, #threads: 1)" ++
  "-------------------------------------------------------------------" ++
  "se.exec_start                                :     321880170.687098" ++
  "se.vruntime                                  :           817.197081" ++
  "se.sum_exec_runtime                          :          4167.723920" ++
  "se.statistics.sum_sleep_runtime              :     115543541.232258" ++
  "se.statistics.wait_start                     :             0.000000" ++
  "se.statistics.sleep_start                    :     321880170.687098" ++
  "se.statistics.block_start                    :             0.000000" ++
  "se.statistics.sleep_max                      :          1008.728760" ++
  "se.statistics.block_max                      :          3613.503856" ++
  "se.statistics.exec_max                       :             4.019296" ++
  "se.statistics.slice_max                      :             4.121777" ++
  "se.statistics.wait_max                       :            16.121293" ++
  "se.statistics.wait_sum                       :           614.212315" ++
  "se.statistics.wait_count                     :               116171" ++
  "se.statistics.iowait_sum                     :             5.531804" ++
  "se.statistics.iowait_count                   :                   10" ++
  "se.nr_migrations                             :                24284" ++
  "se.statistics.nr_migrations_cold             :                    0" ++
  "se.statistics.nr_failed_migrations_affine    :                    0" ++
  "se.statistics.nr_failed_migrations_running   :                  744" ++
  "se.statistics.nr_failed_migrations_hot       :                   40" ++
  "se.statistics.nr_forced_migrations           :                    4" ++
  "se.statistics.nr_wakeups                     :               115512" ++
  "se.statistics.nr_wakeups_sync                :                   21" ++
  "se.statistics.nr_wakeups_migrate             :                23773" ++
  "se.statistics.nr_wakeups_local               :                91682" ++
  "se.statistics.nr_wakeups_remote              :                23830" ++
  "se.statistics.nr_wakeups_affine              :                   71" ++
  "se.statistics.nr_wakeups_affine_attempts     :                   80" ++
  "se.statistics.nr_wakeups_passive             :                    0" ++
  "se.statistics.nr_wakeups_idle                :                    0" ++
  "avg_atom                                     :             0.036034" ++
  "avg_per_cpu                                  :             0.171624" ++
  "nr_switches                                  :               115660" ++
  "nr_voluntary_switches                        :               115513" ++
  "nr_involuntary_switches                      :                  147" ++
  "se.load.weight                               :                 1024" ++
  "se.avg.load_sum                              :                26624" ++
  "se.avg.util_sum                              :                26624" ++
  "se.avg.load_avg                              :                    0" ++
  "se.avg.util_avg                              :                    0" ++
  "se.avg.last_update_time                      :      321880170687098" ++
  "policy                                       :                    0" ++
  "prio                                         :                  120" ++
  "clock-delta                                  :                   29" ++
  "mm->numa_scan_seq                            :                    0" ++
  "numa_pages_migrated                          :                    0" ++
  "numa_preferred_nid                           :                   -1" ++
  "total_numa_faults                            :                    0" ++
  "current_node=0, numa_group_id=0" ++
  "numa_faults node=0 task_private=0 task_shared=0 group_private=0 group_shared=0"


schedFileNoMatch :: String
schedFileNoMatch =
  "-------------------------------------------------------------------" ++
  "se.exec_start                                :     321880170.687098" ++
  "se.vruntime                                  :           817.197081" ++
  "se.sum_exec_runtime                          :          4167.723920" ++
  "se.statistics.sum_sleep_runtime              :     115543541.232258" ++
  "se.statistics.wait_start                     :             0.000000" ++
  "se.statistics.sleep_start                    :     321880170.687098" ++
  "se.statistics.block_start                    :             0.000000" ++
  "se.statistics.sleep_max                      :          1008.728760" ++
  "se.statistics.block_max                      :          3613.503856" ++
  "se.statistics.exec_max                       :             4.019296" ++
  "se.statistics.slice_max                      :             4.121777" ++
  "se.statistics.wait_max                       :            16.121293" ++
  "se.statistics.wait_sum                       :           614.212315" ++
  "se.statistics.wait_count                     :               116171" ++
  "se.statistics.iowait_sum                     :             5.531804" ++
  "se.statistics.iowait_count                   :                   10" ++
  "se.nr_migrations                             :                24284" ++
  "se.statistics.nr_migrations_cold             :                    0" ++
  "se.statistics.nr_failed_migrations_affine    :                    0" ++
  "se.statistics.nr_failed_migrations_running   :                  744" ++
  "se.statistics.nr_failed_migrations_hot       :                   40" ++
  "se.statistics.nr_forced_migrations           :                    4" ++
  "se.statistics.nr_wakeups                     :               115512" ++
  "se.statistics.nr_wakeups_sync                :                   21" ++
  "se.statistics.nr_wakeups_migrate             :                23773" ++
  "se.statistics.nr_wakeups_local               :                91682" ++
  "se.statistics.nr_wakeups_remote              :                23830" ++
  "se.statistics.nr_wakeups_affine              :                   71" ++
  "se.statistics.nr_wakeups_affine_attempts     :                   80" ++
  "se.statistics.nr_wakeups_passive             :                    0" ++
  "se.statistics.nr_wakeups_idle                :                    0" ++
  "avg_atom                                     :             0.036034" ++
  "avg_per_cpu                                  :             0.171624" ++
  "nr_switches                                  :               115660" ++
  "nr_voluntary_switches                        :               115513" ++
  "nr_involuntary_switches                      :                  147" ++
  "se.load.weight                               :                 1024" ++
  "se.avg.load_sum                              :                26624" ++
  "se.avg.util_sum                              :                26624" ++
  "se.avg.load_avg                              :                    0" ++
  "se.avg.util_avg                              :                    0" ++
  "se.avg.last_update_time                      :      321880170687098" ++
  "policy                                       :                    0" ++
  "prio                                         :                  120" ++
  "clock-delta                                  :                   29" ++
  "mm->numa_scan_seq                            :                    0" ++
  "numa_pages_migrated                          :                    0" ++
  "numa_preferred_nid                           :                   -1" ++
  "total_numa_faults                            :                    0" ++
  "current_node=0, numa_group_id=0" ++
  "numa_faults node=0 task_private=0 task_shared=0 group_private=0 group_shared=0"
