unit idetester_tests;

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

{
This is a set of tests with which to test the test runner.
This unit should only be included in a project that is testing the test runner
}
interface

uses
  Classes, SysUtils,
  FPCUnit, TestRegistry,
  idetester_base;

type
  { TTestCaseTests }

  TTestCaseTests = class (TTestCase)
  published
    procedure testPass;
    procedure testFail;
    procedure testError;
    procedure testIgnore;
  end;

implementation


{ TTestCaseTests }

procedure TTestCaseTests.testPass;
begin
  AssertTrue(true);
end;

procedure TTestCaseTests.testFail;
begin
  AssertTrue('message', false);
end;

procedure TTestCaseTests.testError;
begin
  raise EIDETester.create('Error');
end;

procedure TTestCaseTests.testIgnore;
begin
  ignore('testing');
end;


procedure RegisterTests;
begin
  RegisterTest('Library.TestCase tests', TTestCaseTests.Suite);
end;


initialization
  registerTests;
end.

