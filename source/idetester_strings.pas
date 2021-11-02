unit idetester_strings;

{
Copyright (c) 2011+, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.
 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.
 * Neither the name of HL7 nor the names of its contributors may be used to
   endorse or promote products derived from this software without specific
   prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS 'AS IS' AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.
}

{$MODE DELPHI}

interface

uses
  Classes, SysUtils;

const
  helpUrl = 'https://github.com/grahamegrieve/lazarus-ide-tester';

resourcestring
  rs_IdeTester_Name = 'TestCaseView';
  rs_IdeTester_Caption = 'FPCUnit &Test Cases';
  rs_IdeTester_Caption_View = 'Tests';

  rs_IdeTester_Caption_Close = 'Close';
  rs_IdeTester_Caption_Cancel = 'Cancel';
  rs_IdeTester_Caption_OK = 'OK';
  rs_IdeTester_Caption_Help = 'Help';

  rs_IdeTester_Caption_Debug = 'Debug Parameters';
  rs_IdeTester_Caption_Debug_Copy = 'Copy';
  rs_IdeTester_Caption_Debug_Open = '.. Folder';
  rs_IdeTester_Caption_Debug_Label_Main = 'At this time, debugging directly is not supported; instead, you can copy these parameters and run manually in the IDE';
  rs_IdeTester_Caption_Debug_Label_Run_Selection = 'Parameters to execute current selected test';
  rs_IdeTester_Caption_Debug_Label_Run_Checked = 'Parameters to execute current checked tests';
  rs_IdeTester_Caption_Debug_Label_Load = 'Parameters to load tests';
  rs_IdeTester_Caption_Debug_Label_Exec = 'Executable File';

  rs_IdeTester_Caption_Options_Label_Tester = 'Test Project (Blank = this project is the tester)';
  rs_IdeTester_Caption_Options_Label_Parameters = 'Additional Execution Parameters';
  rs_IdeTester_Caption_Options_Label_Timeout = 'Time to wait before killling tests (ms)';
  rs_IdeTester_Caption_Options_Label_AutoSave = 'Autosave Source Files before running tests';

  rs_IdeTester_Err_No_Project = 'No Project loaded to test';
  rs_IdeTester_Err_Project_Type = 'Projects of type %s cannot be tested';
  rs_IdeTester_Err_Project_Target = 'Unable to retrieve target file of project';
  rs_IdeTester_Err_Node_Not_Found = 'Node %s not found!';
  rs_IdeTester_Err_No_Tests = 'No Tests Checked';
  rs_IdeTester_Err_No_FailedTests = 'No Failed Tests';
  rs_IdeTester_Err_No_Load_Tests = 'Unable to load tests. Check that the project runs the tests correctly (see %s)';
  rs_IdeTester_Err_Project_Not_Found = 'Test Project %s not found';
  rs_IdeTester_Err_LazBuild_Not_Found = 'LazBuild not found at %s';
  rs_IdeTester_Err_LazBuild_Failed = 'Compiling test project failed - see %s for log';
  rs_IdeTester_Err_LazBuild_Error = 'Error compiling test project : %s (line %s). See %s for full log';
  rs_IdeTester_Err_LazBuild_No_ExeName = 'Compiling test project succeded, but unable to find executable name - see %s for log';
  rs_IdeTester_Err_LazBuild_No_Exe = 'Compiling test project succeded, but unable to find executable file %s - see %s for log';

  rs_IdeTester_Msg_AllTests = 'All Tests';
  rs_IdeTester_Msg_Running_Test = 'Running Test ';
  rs_IdeTester_Msg_Process_Terminated = 'Process terminated!';
  rs_IdeTester_Msg_NOT_SUPPORTED = 'not supported here';
  rs_IdeTester_Msg_Starts = 'Starts';
  rs_IdeTester_Msg_Ends = 'Ends';
  rs_IdeTester_Msg_Compiling = 'Compiling %s';
  rs_IdeTester_Msg_Loading = 'Loading';

  rs_IdeTester_PBar_Runs = 'Tests: %s/%s';
  rs_IdeTester_PBar_Errors = '%s    Errors: %s';
  rs_IdeTester_PBar_Failures = '%s     Failures: %s';
  rs_IdeTester_PBar_Time = '%s     Time: %ss...';
  rs_IdeTester_PBar_TimeDone = '%s     Time: %ss';

  rs_IdeTester_SBar_Tests = 'Tests';
  rs_IdeTester_SBar_Checked = 'checked';
  rs_IdeTester_SBar_Passed = 'passed';
  rs_IdeTester_SBar_Failed = 'failed';
  rs_IdeTester_SBar_Errors = 'errors';
  rs_IdeTester_SBar_NotRun = 'not run';
  rs_IdeTester_SBar_All_OK = 'All OK✓';

  rs_IdeTester_Caption_DebugSelected_NODE = 'Debug Selected Test Suite';
  rs_IdeTester_Caption_RunSelected_NODE = 'Run Selected Test Suite';
  rs_IdeTester_Caption_Copy_NODE = 'Copy Test Suite Results to Clipboard';
  rs_IdeTester_Caption_Reset_NODE = 'Clear Test Suite Results';

  rs_IdeTester_Caption_DebugSelected_LEAF = 'Debug Selected Test';
  rs_IdeTester_Caption_RunSelected_LEAF = 'Run Selected Test';
  rs_IdeTester_Caption_Copy_LEAF = 'Copy Test Result to Clipboard';
  rs_IdeTester_Caption_Reset_LEAF = 'Clear Test Results';

  rs_IdeTester_Caption_Configure = 'Configuration Options';
  rs_IdeTester_Caption_Reload = 'Reload Test List';
  rs_IdeTester_Caption_Stop = 'Stop Running Tests';
  rs_IdeTester_Caption_RunFailed = 'Run Failed Tests';
  rs_IdeTester_Caption_RunChecked = 'Run All Checked Tests';
  rs_IdeTester_Caption_ViewFlat = '&Without Heirarchy';
  rs_IdeTester_Caption_ViewIssues = '&Failed';
  rs_IdeTester_Caption_ViewUnrun = '&Not yet run';
  rs_IdeTester_Caption_ViewAll = 'Full &Heirarchy';


  rs_IdeTester_Hint_DebugSelected = 'Debug Selected';
  rs_IdeTester_Hint_Configure = 'Set Configuration Options';
  rs_IdeTester_Hint_Reload = 'Load the list of tests again';
  rs_IdeTester_Hint_SelectAll = 'Check selected test(s)';
  rs_IdeTester_Hint_UnselectAll = 'Uncheck selected Tests';
  rs_IdeTester_Hint_Reset = 'Clear All Test Results';
  rs_IdeTester_Hint_Copy = 'Copy Results to Clipboard';
  rs_IdeTester_Hint_Stop = 'Stop running tests';
  rs_IdeTester_Hint_RunFailed = 'Re-run all failed tests (including errors)';
  rs_IdeTester_Hint_RunChecked = 'Run All Checked Tests';
  rs_IdeTester_Hint_RunSelected = 'Run Selected test + children';
  rs_IdeTester_Hint_ViewFlat = 'View all tests without heirarchy';
  rs_IdeTester_Hint_ViewIssues = 'View only tests that failed';
  rs_IdeTester_Hint_ViewUnrun = 'View Tests not yet run';
  rs_IdeTester_Hint_ViewAll = 'View the Full Heirarchy of tests';

  rs_IdeTester_ProjectType_None = 'None';
  rs_IdeTester_ProjectType_Program = 'Program';
  rs_IdeTester_ProjectType_Library = 'Library';
  rs_IdeTester_ProjectType_Package = 'Package';
  rs_IdeTester_ProjectType_Unit = 'Unit';

implementation

end.

