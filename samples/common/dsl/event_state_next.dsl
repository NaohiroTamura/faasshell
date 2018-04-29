fsm([event('HelloWorld',"frn::states:::event:test",[timeout_seconds(5)]),
     pass('Final State', [])]).
