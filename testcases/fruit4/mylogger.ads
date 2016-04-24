with AdaBase.Logger.Base;

package MyLogger is

   package ALB renames AdaBase.Logger.Base;

   type CustomLogger is new ALB.Base_Logger and ALB.AIL.iLogger
      with null record;

   overriding
   procedure reaction (listener : CustomLogger);

   clogger : aliased CustomLogger;

end MyLogger;
