module Main (main) where

import Test.Hspec

import Types
import Parser
import Interpreter

-- | Test utility function
t :: String -> String -> Spec
t input expected = it (show input ++ " => " ++ expected) $ do
  case parseProgram input of
    Left err -> expectationFailure $ "Parse error: " ++ show err
    Right tokens -> case executeProgram tokens initialState of
      Left err -> expectationFailure $ "Execution error: " ++ show err
      Right (_, result) -> show result `shouldBe` expected

main :: IO ()
main = hspec $ do
  describe "official tests for non-error programs" $ do
    {-- literals -}
    t "3"                           "3"
    t "121231324135634563456363567" "121231324135634563456363567"
    t "1.0"                         "1.0"
    t "0.0"                         "0.0"
    t "-1"                          "-1"
    t "-1.1"                        "-1.1"
    t "False"                       "False"
    t "True"                        "True"
    t "[ [ ] [ ] ]"                 "[[],[]]"
    t "[ False [ ] True [ 1 2 ] ]"  "[False,[],True,[1,2]]"
    t "\" [ so { not if ] and } \"" "\"[ so { not if ] and }\""

    {-- quotation literals -}
    t "{ 20 10 + }"             "{ 20 10 + }"
    t "[ { + } { 10 + } { 20 10 + } ]"   "[{ + },{ 10 + },{ 20 10 + }]"
    
    {-- simple arithmetic -}
    t "1 1 +"               "2"       
    t "10 20 *"             "200"
    t "20 2 div"            "10"
    t "20 2 /"              "10.0"
    
    {-- arithmetic with type coercion -}
    t "1 1.0 +"             "2.0"       
    t "10 20.0 *"           "200.0"
    t "20 2.0 div"          "10"
    t "20.0 2.0 div"        "10"
    
    {-- bool operations -}
    t "False False &&"      "False"
    t "False True ||"       "True"
    t "False not"           "True"
    t "True not"            "False"
    
    {-- comparisons -}
    t "20 10 <"             "False"
    t "20 10 >"             "True"
    t "20 10.0 >"           "True"
    t "20.0 20.0 >"         "False"
    t "10 10 =="            "True"
    t "10 10.0 =="          "True"
    t "True True =="        "True"
    t "True 40 40 == =="    "True"
    t "\" abba \" \" abba \" ==" "True"
    t "[ ] [ ] =="          "True"
    t "[ 1 2 ] [ 1 2 ] =="  "True"
    t " [ [ ] ] [ [ ] ] ==" "True"
    
    {-- stack operations -}
    t "10 20 swap pop"          "20"
    t "10 dup dup + swap pop"   "20"
    t "10 20 swap dup + div"    "1"
    
    {-- length -}
    t "\" hello \" length"              "5"
    t "\" hello world \" length"        "11"
    t "[ 1 2 3 [ ] ] length"            "4"
    t "{ 10 20 + } length"              "3"

    {-- String parsing -}
    t "\" 12 \" parseInteger"           "12"
    t "\" 12.34 \" parseFloat"          "12.34"
    
    {-- lists -}
    t "[ 1 2 3 ]"           "[1,2,3]"
    t "[ 1 \" bob \" ]"     "[1,\"bob\"]"
    t "[ 1 2 ] empty"       "False"
    t "[ ] empty"           "True"
    t "[ 1 2 3 ] head"      "1"
    t "[ 1 2 3 ] length"    "3"
    t "[ 1 2 3 ] tail"      "[2,3]"
    t "1 [ ] cons"          "[1]"
    t "1 [ 2 3 ] cons"      "[1,2,3]"
    t "[ 1 ] [ 2 3 ] append" "[1,2,3]"
    t "[ 1 2 ] [ ] append"  "[1,2]"
    t "[ 1 ] [ 2 3 ] cons"  "[[1],2,3]"

    {-- assignments -}
    t "age"                             "age"
    t "age 10 := age"                   "10"
    t "10 age swap := age"              "10"
    t "[ 1 2 3 ] list swap := list"     "[1,2,3]"
    t "age 20 := [ 10 age ]"            "[10,20]"

    t "inc { 1 + } fun 1 inc"           "2"
    t "mul10 { 10 * } fun inc { 1 + } fun 10 inc mul10" "110"
    
    {-- quotations -}
    t "{ 20 10 + } exec"                "30"
    t "10 { 20 + } exec"                "30"
    t "10 20 { + } exec"                "30"
    t "{ { 10 20 + } exec } exec"       "30"
    t "{ { 10 20 + } exec 20 + } exec"  "50"
    
    {-- if -}
    t "True if { 20 } { }"               "20"
    t "True if { 20 10 + } { 3 }"        "30"
    t "10 5 5 == if { 10 + } { 100 + }"  "20"
    t "False if { } { 45 }"              "45"
    t "True if { False if { 50 } { 100 } } { 30 }" "100"

  describe "error handling" $ do
    it "handles stack underflow" $ do
      case parseProgram "pop" of
        Left _ -> expectationFailure "Parse error"
        Right tokens -> case executeProgram tokens initialState of
          Left StackEmpty -> pure ()
          Left err -> expectationFailure $ "Expected StackEmpty, got: " ++ show err
          Right _ -> expectationFailure "Expected error, got success"

    it "handles division by zero" $ do
      case parseProgram "10 0 div" of
        Left _ -> expectationFailure "Parse error"
        Right tokens -> case executeProgram tokens initialState of
          Left DivisionByZero -> pure ()
          Left err -> expectationFailure $ "Expected DivisionByZero, got: " ++ show err
          Right _ -> expectationFailure "Expected error, got success"

    it "handles type errors" $ do
      case parseProgram "True 10 +" of
        Left _ -> expectationFailure "Parse error"
        Right tokens -> case executeProgram tokens initialState of
          Left (ExpectedBoolOrNumber _) -> pure ()
          Left err -> expectationFailure $ "Expected ExpectedBoolOrNumber, got: " ++ show err
          Right _ -> expectationFailure "Expected error, got success"