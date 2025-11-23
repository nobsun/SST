# app/Main.hs

単純な対話プログラムの雛形

対話プログラムは `responder :: String -> ([String] -> [String])` 関数で表現される。
`responder` の実態は、`MachineState` 上の状態遷移系である。

初期状態を生成する `initial :: String -> [String] -> MachineState` および `eval :: MachineState -> [MachineState]` の下請けである `isFinal :: MachineState -> Bool` と `step :: MachineState -> MachineState` を実装すればよい。
