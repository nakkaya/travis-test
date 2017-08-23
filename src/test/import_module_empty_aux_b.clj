
(require '[modules.module-a :as mod-a])
(require '[modules.module-b :as mod-b])

(native-declare "const int XYZ_SIZE = 123;")
(native-declare "int xyz_arr[XYZ_SIZE];")
