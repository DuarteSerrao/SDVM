"""Python3 Optimized implementation of Bubble sort
"""
arr = [3449, 688, 1328, 1821, 2947, 3906, 4534, 4417, 3498, 464, 4071, 2471, 3539, 1044, 3119, 3021, 2291, 150, 3890, 505, 3120, 834, 3988, 860, 1609, 3582, 1224, 4904, 2605, 3350, 848, 3656, 4581, 3845, 2207, 428, 4923, 3246, 4494, 614, 3473, 3544, 1671, 1489, 2985, 3218, 3809, 3670, 1523, 4563, 3825, 2588, 2013, 1302, 4269, 3148, 4558, 1876, 4903, 1015, 3159, 2282, 1697, 3973, 2494, 2688, 4016, 4192, 2805, 3073, 1960, 816, 3241, 3069, 955, 1877, 2911, 333, 782, 2862, 4968, 4974, 4822, 1733, 4419, 4001, 1181, 1377, 369, 554, 863, 2834, 2457, 4318, 4661, 1019, 1259, 2554, 4199, 4367, 2038, 748, 4987, 1290, 1588, 871, 2891, 3300, 910, 3492, 966, 2836, 2335, 1527, 1826, 2968, 4510, 303, 3364, 4103, 4955, 3259, 2769, 3813, 1759, 2027, 3665, 1534, 3956, 807, 2337, 838, 1511, 4414, 1587, 4424, 4125, 3926, 2072, 1251, 2945, 667, 172, 1475, 4331, 3872, 4036, 3338, 2281, 2224, 4137, 872, 745, 3471, 1232, 3478, 4224, 4958, 4664, 395, 4951, 2007, 886, 3508, 2626, 4776, 97, 1893, 2357, 995, 1119, 712, 4316, 1294, 3518, 4319, 2848, 2450, 1521, 2990, 2079, 179, 560, 364, 4969, 3004, 799, 3233, 4203, 207, 4967, 3914, 598, 2229, 1988, 4110, 3460, 1399, 37, 608, 1076, 3839, 3520, 2298, 1083, 601, 4654, 597, 4064, 1680, 2585, 2619, 2665, 2307, 2642, 3803, 1774, 4223, 3316, 3946, 1466, 2695, 1606, 4270, 1344, 2070, 4283, 1937, 3762, 1345, 4725, 1062, 3668, 1349, 4541, 1764, 4716, 3067, 190, 3545, 3984, 510, 1567, 3513, 1210, 2842, 2315, 86, 2439, 4649, 1840, 4393, 4885, 1212, 1236, 2535, 4453, 4433, 729, 1904, 691, 1001, 4266, 1664, 4323, 1705, 3826, 3401, 4975, 1827, 951, 1336, 2576, 2643, 436, 753, 4863, 3124, 1919, 1518, 1111, 2869, 4907, 2489, 4225, 2957, 636, 1011, 1263, 3321, 756, 3595, 1672, 1711, 3694, 3542, 996, 430, 3579, 2491, 811, 1691, 704, 4855, 3505, 4724, 1009, 1191, 488, 4052, 1538, 2756, 1356, 36, 3057, 4762, 1550, 4670, 6, 3220, 4333, 2547, 46, 445, 4462, 4037, 793, 31, 1910, 3171, 474, 4705, 3634, 861, 3879, 2267, 3600, 2250, 1825, 3065, 867, 1025, 3641, 907, 1166, 512, 3793, 3651, 606, 2476, 363, 4154, 4391, 2448, 2286, 2803, 3687, 310, 4655, 2314, 960, 4309, 1440, 4411, 747, 1245, 4770, 4247, 3892, 4408, 2057, 723, 4733, 4657, 2047, 533, 3997, 3374, 93, 1205, 2545, 415, 2989, 17, 2396, 3466, 4027, 2431, 1714, 3812, 3713, 177, 631, 3562, 3533, 3348, 4181, 972, 105, 3003, 1220, 411, 3636, 3991, 2819, 280, 159, 275, 1464, 1431, 1446, 4428, 1647, 2164, 1180, 4596, 3951, 899, 1284, 248, 803, 2496, 4076, 1465, 2657, 1884, 2454, 3913, 1756, 4406, 2186, 4839, 519, 964, 1750, 4277, 997, 1323, 3372, 3167, 2260, 1260, 3731, 1702, 3654, 4405, 2205, 2197, 3116, 2192, 2503, 1286, 2287, 3045, 1822, 2691, 537, 3335, 2076, 3379, 2766, 2937, 946, 4815, 3952, 3197, 623, 393, 130, 9, 3446, 1068, 875, 4382, 3123, 1097, 1894, 184, 4222, 2807, 1443, 3223, 231, 1412, 4648, 1372, 2974, 1408, 3039, 4023, 3569, 3193, 2689, 1432, 328, 3287, 2574, 2397, 4376, 2232, 3805, 2686, 4608, 2116, 3157, 2080, 4692, 1281, 2273, 2105, 3993, 2132, 1462, 3030, 2854, 952, 654, 1008, 1997, 4275, 1800, 3915, 1339, 3461, 257, 2235, 4888, 2122, 1013, 4592, 926, 217, 2815, 4651, 2092, 806, 2983, 480, 580, 1136, 1947, 4058, 2014, 2530, 3177, 3477, 1351, 3095, 4200, 4660, 2631, 779, 1498, 4444, 1849, 4090, 1381, 3250, 4509, 676, 780, 1060, 481, 1864, 1787, 2870, 3306, 461, 3068, 422, 2938, 3179, 2333, 3552, 4812, 4742, 3180, 2872, 2016, 2464, 4677, 2285, 2075, 4798, 2126, 2194, 4347, 2248, 3497, 655, 293, 1747, 1758, 1452, 2342, 90, 3485, 4557, 2187, 2953, 998, 1820, 1270, 4447, 4127, 2603, 1052, 2375, 2455, 1022, 3079, 1776, 1104, 1318, 1269, 2919, 2679, 1468, 2726, 2240, 4380, 2066, 2214, 521, 1246, 1255, 1463, 3495, 2946, 4601, 377, 4633, 3827, 3591, 2678, 4920, 3966, 3578, 2405, 4437, 2446, 2006, 1620, 885, 4083, 30, 4242, 2005, 4653, 2023, 3725, 1449, 1545, 3903, 2569, 1437, 758, 3983, 1743, 1812, 2053, 1283, 3722, 1866, 1968, 2634, 3797, 4415, 4613, 4869, 2713, 435, 2216, 1088, 4021, 4597, 754, 4622, 3404, 3238, 1755, 4678, 273, 4156, 2351, 4981, 3937, 1744, 4038, 2493, 4531, 4074, 4431, 864, 4281, 2060, 1149, 2223, 4455, 2118, 292, 89, 73, 529, 4182, 2456, 4905, 2901, 2262, 894, 2136, 4834, 3701, 532, 2606, 4671, 3674, 4298, 2548, 4149, 3483, 516, 3323, 4565, 3739, 1650, 325, 4797, 3397, 2371, 2699, 1469, 3840, 2188, 4335, 3418, 2673, 1938, 259, 3910, 3239, 1506, 4303, 1554, 789, 4694, 162, 1851, 1809, 2226, 3234, 4485, 2652, 1189, 988, 1287, 164, 1722, 484, 4306, 1248, 4995, 187, 2146, 3524, 2324, 173, 3380, 1798, 923, 917, 1361, 1842, 1369, 3416, 3833, 792, 2181, 1138, 4579, 4019, 211, 3635, 3186, 1115, 330, 2600, 1736, 4803, 2107, 403, 4959, 1546, 4690, 1121, 4949, 289, 1889, 3423, 630, 296, 4219, 3293, 785, 3440, 3016, 3788, 4638, 2130, 3692, 3738, 1900, 4722, 2811, 583, 4235, 702, 4398, 866, 4374, 1380, 3129, 847, 2033, 2131, 2189, 903, 3606, 450, 3125, 368, 2274, 2326, 4515, 3019, 336, 3734, 3025, 4482, 1962, 2170, 2598, 2400, 3326, 222, 3883, 434, 3263, 4378, 4353, 975, 4880, 4928, 3710, 2090, 1944, 1080, 790, 3860, 3320, 815, 3608, 4429, 4804, 2073, 572, 2325, 3153, 210, 4434, 4128, 2783, 2031, 1873, 3846, 3061, 1065, 4954, 356, 309, 2008, 1675, 2003, 1699, 290, 2358, 3989, 947, 967, 1048, 1837, 3815, 344, 401, 2637, 4440, 2483, 3938, 1706, 3559, 1943, 3689, 3782, 3168, 3775, 3568, 2887, 3134, 2463, 1020, 1395, 4859, 84, 4851, 3208, 4849, 3202, 3698, 4508, 3676, 2408, 1727, 2479, 2793, 3343, 2561, 1676, 1354, 3268, 2681, 3355, 1018, 3405, 3396, 3980, 3070, 4590, 3711, 2553, 1416, 4492, 4930, 4320, 244, 4937, 2102, 355, 192, 4555, 1779, 1654, 3337, 4315, 4439, 956, 3688, 2119, 1420, 645, 313, 2470, 4086, 2843, 550, 1698, 3301, 3328, 4244, 1026, 2019, 3527, 4387, 3135, 2841, 2015, 4466, 3658, 3071, 2310, 4243, 263, 3586, 1946, 2182, 2708, 829, 3366, 3537, 3638, 1474, 2711, 320, 2370, 1669, 4139, 590, 2376, 1271, 2108, 4805, 1667, 2847, 3034, 4044, 2926, 3277, 3987, 858, 2732, 2867, 2720, 3682, 1882, 3191, 1346, 750, 1661, 3918, 2808, 4427, 2527, 1230, 2584, 1092, 544, 3000, 1607, 2744, 3521, 2899, 4850, 3342, 3838, 2716, 2907, 437, 879, 509, 839, 4088, 755, 2647, 3790, 1235, 4075, 4310, 52, 561, 3886, 4632, 4564, 4521, 637, 4615, 2921, 233, 272, 4856, 965, 4325, 3346, 3893, 223, 4448, 99, 709, 4179, 2, 3584, 3796, 3334, 2955, 4791, 4130, 2369, 4377, 3643, 4506, 1956, 127, 4276, 1907, 2551, 2538, 3187, 490, 1458, 4717, 2233, 4317, 2230, 58, 475, 2552, 2402, 2543, 4089, 4749, 424, 1569, 4799, 4621, 3228, 1023, 1002, 3360, 2925, 3087, 485, 3408, 3780, 3917, 4789, 2610, 2802, 4736, 994, 1892, 3281, 2137, 3581, 3776, 3044, 2864, 659, 766, 4240, 1400, 1666, 3867, 358, 4231, 1745, 1602, 918, 3376, 2362, 1470, 1643, 1438, 402, 4024, 4473, 1881, 1289, 2566, 1183, 1496, 256, 2055, 1619, 569, 3028, 3424, 1843, 2599, 4827, 2308, 4215, 4676, 4143, 2526, 4048, 3900, 110, 388, 1253, 4893, 818, 2150, 1250, 3510, 1817, 2746, 3251, 3203, 2292, 3459, 3415, 199, 75, 1586, 575, 851, 2162, 4924, 4672, 3804, 4073, 3561, 4738, 3781, 2620, 3205, 1686, 2034, 891, 897, 2129, 4070, 1833, 1414, 291, 2169, 220, 1955, 3553, 4760, 3435, 943, 3743, 2265, 59, 4129, 2299, 2722, 1033, 429, 4679, 2719, 3041, 4569, 83, 2949, 2601, 1039, 1311, 494, 2816, 4059, 43, 757, 1299, 359, 633, 389, 2430, 182, 4794, 2748, 2624, 3103, 1636, 979, 767, 3141, 2941, 3181, 737, 1055, 674, 3432, 3336, 4927, 2085, 1094, 4800, 111, 4352, 3295, 2004, 1611, 2276, 4639, 3142, 165, 1844, 585, 2196, 4527, 3351, 321, 499, 3920, 1792, 1313, 1653, 2026, 2173, 4931, 3273, 2684, 3672, 2256, 3292, 689, 3610, 4902, 2367, 18, 1167, 1321, 2424, 4157, 2536, 721, 2466, 4096, 4587, 2765, 3808, 1109, 201, 2183, 2052, 4092, 678, 4131, 990, 1917, 104, 2674, 4028, 1589, 4890, 42, 4891, 3806, 4349, 4843, 1073, 4108, 736, 2506, 4050, 2762, 4548, 2087, 2018, 929, 439, 326, 622, 1560, 2962, 3708, 3200, 1964, 2407, 4802, 4039, 1279, 240, 2236, 4363, 1229, 842, 752, 1695, 4866, 1031, 1928, 2894, 1593, 2247, 2486, 3881, 4892, 3462, 824, 1959, 2855, 2449, 2221, 4162, 1488, 414, 1335, 486, 4226, 2213, 3402, 2113, 2010, 3640, 3312, 1870, 4971, 2481, 1531, 3107, 4999, 2668, 1306, 1969, 4032, 1629, 254, 2568, 4068, 2117, 1120, 1972, 774, 3496, 2658, 1950, 915, 308, 2202, 534, 2525, 2460, 3727, 1347, 81, 2795, 4362, 2730, 1845, 1895, 1999, 4344, 4049, 4539, 1124, 1098, 1852, 1304, 1243, 568, 957, 3712, 1160, 1604, 1936, 511, 4381, 2112, 133, 53, 4383, 1450, 921, 3969, 4546, 392, 1761, 2542, 541, 2648, 3724, 2579, 1355, 4081, 1320, 4004, 4808, 3570, 1187, 1563, 28, 3043, 299, 3063, 360, 1862, 884, 751, 3026, 1193, 2717, 2540, 4665, 2228, 60, 2353, 4296, 3944, 3934, 4282, 3445, 4751, 944, 3947, 4168, 4324, 324, 3209, 2305, 3817, 1455, 2275, 197, 1789, 44, 2967, 8, 1291, 4246, 3822, 2270, 2936, 390, 549, 1433, 2559, 3084, 2518, 1308, 2425, 2987, 1387, 4402, 2268, 3467, 2671, 1410, 1482, 4754, 2759, 2771, 1748, 4647, 1071, 2616, 4350, 890, 4178, 2009, 122, 2437, 3794, 4765, 859, 4872, 2591, 1288, 887, 2582, 3059, 4645, 1258, 2690, 2482, 1807, 3703, 2115, 4258, 764, 878, 1564, 2120, 115, 4232, 3411, 4220, 1402, 3062, 2784, 4858, 323, 4395, 2372, 703, 4813, 209, 1135, 455, 205, 3574, 2244, 189, 2440, 1200, 3799, 1716, 347, 2662, 3216, 1603, 229, 3807, 4268, 252, 2997, 518, 4963, 2812, 613, 3933, 397, 471, 2948, 4807, 4952, 3160, 381, 849, 1660, 869, 1689, 151, 2393, 1573, 2303, 2763, 3820, 4695, 2056, 949, 3169, 765, 4982, 2318, 2775, 1134, 1995, 3378, 1176, 357, 744, 524, 4552, 4084, 1766, 4445, 3623, 1890, 4371, 1646, 2939, 3529, 1772, 1509, 1803, 1422, 805, 1147, 1145, 3265, 4474, 4656, 1206, 2971, 2349, 2656, 3802, 4189, 740, 530, 236, 3447, 206, 1063, 2433, 963, 261, 3671, 4354, 3137, 2166, 4766, 4909, 1808, 1958, 4477, 602, 3017, 4278, 2804, 895, 3506, 2411, 4404, 1763, 3365, 2994, 463, 1066, 379, 1215, 487, 2104, 2532, 341, 1070, 738, 448, 2331, 1926, 3037, 760, 4652, 3786, 3862, 4562, 1312, 1818, 574, 3522, 586, 4085, 1814, 2680, 3753, 3563, 2625, 2300, 1034, 1303, 2124, 1720, 34, 3931, 1841, 1975, 1652, 4704, 1106, 2621, 460, 3538, 2301, 1315, 930, 3729, 2687, 4536, 3837, 4896, 3566, 2718, 1300, 2638, 1768, 3995, 4729, 3451, 1990, 3198, 4895, 1038, 1146, 1902, 4263, 4416, 4910, 3777, 4916, 4167, 1785, 2809, 370, 2522, 2993, 4478, 4217, 1934, 4208, 4583, 1515, 335, 2868, 4575, 24, 4020, 4254, 4450, 2419, 1685, 2539, 3602, 1984, 181, 345, 3488, 2931, 4976, 1824, 3516, 4340, 1142, 3686, 4875, 3898, 3437, 1883, 4072, 3064, 266, 2653, 3863, 4214, 224, 4144, 3302, 698, 1459, 1014, 4543, 153, 2078, 4407, 1093, 404, 4842, 2710, 1516, 609, 1004, 1024, 2195, 4764, 1642, 4454, 1233, 2863, 2100, 1403, 4847, 1305, 2992, 4607, 108, 1640, 2519, 1903, 2651, 3593, 2502, 2398, 925, 3789, 1525, 1153, 2725, 3778, 2390, 2410, 4051, 1577, 4147, 3769, 3663, 3550, 3400, 3507, 868, 3677, 2857, 1941, 1501, 4056, 3810, 2767, 3858, 3213, 1390, 4612, 3269, 4669, 102, 693, 2165, 4609, 935, 276, 255, 1638, 1915, 4877, 3387, 773, 2081, 218, 4436, 1656, 3007, 4998, 1630, 4150, 566, 1426, 1888, 76, 969, 3274, 4938, 3895, 3902, 2743, 3244, 1125, 2041, 4714, 1794, 274, 576, 1207, 1157, 3962, 4229, 2432, 4122, 2204, 821, 3001, 3175, 4045, 3943, 472, 2918, 1596, 4026, 1529, 2810, 3178, 3685, 3150, 877, 2478, 2966, 1396, 2020, 157, 4183, 446, 137, 4942, 3557, 3425, 3617, 797, 3303, 1049, 987, 1330, 125, 2474, 4418, 1989, 438, 1665, 4138, 2817, 1544, 3887, 3136, 3317, 4634, 3113, 4002, 3230, 3012, 41, 3603, 4883, 230, 4993, 3368, 3097, 2898, 1194, 3255, 144, 3556, 1089, 1819, 1610, 1811, 1597, 3112, 491, 306, 2828, 3221, 4631, 378, 928, 2296, 4095, 4461, 819, 1648, 2900, 593, 905, 617, 2617, 214, 442, 3876, 4889, 4523, 1583, 3702, 4098, 405, 694, 2111, 2148, 279, 2618, 3611, 2412, 3975, 731, 2327, 2106, 3761, 527, 4748, 4659, 715, 4464, 976, 1173, 2840, 1170, 2655, 1359, 626, 1605, 4471, 1282, 3601, 2692, 1721, 4572, 4091, 4186, 2302, 3315, 1219, 1003, 332, 1971, 2831, 3105, 2453, 40, 804, 2361, 4646, 3723, 4554, 3106, 2933, 1244, 538, 1708, 2965, 2399, 3619, 3923, 734, 1362, 3077, 3861, 4528, 1992, 2293, 945, 468, 1878, 2288, 2916, 1036, 1108, 51, 4385, 1829, 342, 346, 2915, 1174, 1499, 4342, 3296, 2316, 3512, 4929, 1050, 3237, 1161, 4468, 3576, 80, 4334, 1012, 783, 1266, 4602, 1217, 4481, 3681, 1017, 3929, 4136, 977, 2943, 2820, 2801, 4005, 3530, 4865, 1670, 4250, 2377, 3282, 339, 4459, 4668, 16, 2334, 4299, 579, 727, 4864, 582, 3955, 2404, 2306, 3859, 4921, 2958, 2880, 2885, 1701, 1574, 239, 4423, 772, 3431, 1195, 2257, 175, 3554, 3940, 3853, 739, 380, 641, 1386, 2904, 2851, 4735, 2739, 2683, 4701, 1141, 426, 3060, 4860, 3695, 489, 1687, 2984, 1832, 619, 3152, 1154, 3891, 1565, 4728, 1341, 3744, 2586, 2508, 2514, 786, 3227, 4000, 1047, 639, 4067, 4533, 2304, 2354, 2794, 268, 2253, 3700, 4599, 1398, 2612, 1973, 410, 1922, 3053, 1337, 4698, 1754, 1831, 286, 3333, 4368, 3407, 1548, 1641, 4175, 2208, 3252, 1576, 3873, 1272, 3541, 2754, 4230, 4769, 4737, 2133, 2238, 3490, 1421, 4394, 2697, 2141, 937, 2558, 4826, 3696, 4469, 4321, 1122, 2669, 4911, 3801, 65, 354, 1201, 3932, 4112, 4984, 4114, 2597, 4301, 498, 962, 2513, 4022, 1982, 3515, 4060, 3164, 2734, 2639, 959, 459, 96, 3901, 762, 4830, 2839, 2147, 1742, 4726, 2095, 2444, 3771, 2903, 4069, 938, 4470, 2309, 677, 2636, 2845, 3130, 3684, 2978, 883, 3051, 2507, 543, 3089, 642, 3536, 4681, 3519, 1681, 155, 1983, 3992, 2913, 4611, 1152, 526, 382, 3258, 2344, 4961, 158, 2724, 3721, 4840, 3297, 1214, 1694, 718, 3436, 1796, 4087, 4580, 4624, 1186, 2201, 1419, 2231, 1683, 246, 2480, 1608, 3386, 4184, 3888, 1148, 3040, 528, 1731, 2979, 2861, 4688, 2785, 3994, 2509, 1901, 3523, 4330, 1367, 334, 4502, 1874, 2821, 2991, 2380, 2061, 1267, 2676, 1690, 3453, 3236, 2505, 4838, 3958, 4731, 3655, 2414, 876, 2320, 1970, 2283, 2852, 3210, 881, 3406, 3222, 315, 4925, 4718, 2180, 1371, 4702, 2768, 3620, 4936, 2838, 4293, 4837, 2487, 870, 1280, 465, 4410, 4300, 4054, 1172, 1027, 203, 3525, 4514, 1479, 1139, 3571, 1993, 2633, 1530, 2595, 282, 2366, 4796, 2037, 2822, 4960, 1204, 3163, 3422, 3491, 4873, 556, 4061, 3930, 742, 669, 3362, 3718, 1914, 2705, 4117, 3194, 284, 3565, 2871, 2249, 3792, 3474, 2114, 444, 3772, 2798, 1871, 4715, 1277, 2209, 3924, 1717, 129, 4743, 809, 2151, 3015, 4627, 902, 1342, 121, 1360, 168, 4337, 933, 3304, 4366, 4187, 4195, 4898, 492, 2736, 2908, 1178, 573, 1016, 936, 4279, 504, 4288, 4505, 4761, 4190, 1314, 4106, 3211, 3093, 4080, 3092, 1998, 2738, 2161, 4824, 4099, 3013, 3567, 1966, 1963, 1102, 4831, 577, 2355, 3373, 4643, 1949, 232, 1770, 711, 1730, 3468, 243, 3855, 2001, 3370, 896, 4355, 2383, 722, 2972, 148, 2950, 213, 4227, 3870, 1898, 2319, 298, 128, 557, 4234, 3014, 2272, 2573, 3613, 281, 1752, 245, 2185, 2895, 880, 656, 4009, 794, 312, 4673, 4198, 562, 2740, 2343, 3417, 4421, 270, 551, 2980, 4570, 759, 1514, 1939, 1393, 2589, 4588, 4107, 3145, 416, 4104, 741, 2534, 2172, 1118, 3824, 4356, 1932, 1171, 3074, 4610, 4388, 914, 2664, 2877, 2721, 1096, 3639, 2110, 4210, 2550, 4532, 1239, 4261, 1679, 2849, 3412, 4525, 3111, 1913, 2447, 1228, 3298, 4322, 2245, 3008, 4273, 3598, 2184, 1099, 3419, 687, 3921, 4397, 922, 3961, 349, 3075, 1961, 4185, 2387, 1738, 3217, 103, 2560, 1978, 3648, 2416, 3850, 3760, 3279, 63, 2263, 3907, 3911, 2778, 3963, 4120, 4699, 2297, 3403, 1867, 4806, 547, 3894, 1188, 2995, 2935, 54, 3732, 3642, 1985, 3389, 1777, 2923, 1595, 3653, 2818, 4194, 352, 3339, 3392, 2144, 4475, 3897, 4553, 166, 769, 3382, 4177, 4739, 2954, 2365, 1365, 1709, 4568, 1030, 285, 1273, 3309, 3752, 2823, 1644, 2255, 1880, 3998, 1032, 3384, 1159, 4047, 200, 770, 2575, 1549, 4526, 2590, 697, 4496, 2139, 2865, 1192, 477, 3889, 4360, 3078, 4111, 1069, 235, 3196, 4115, 1202, 4965, 1444, 134, 178, 3056, 2832, 2970, 250, 3140, 77, 2951, 3587, 3866, 3844, 4491, 3945, 1987, 4980, 771, 4605, 1265, 4121, 578, 2615, 4784, 2051, 2099, 4730, 1847, 3347, 449, 1473, 686, 1401, 2490, 2825, 1986, 362, 3046, 1617, 4010, 682, 2996, 2212, 69, 124, 1322, 3501, 4438, 4265, 2649, 4828, 4066, 3646, 3463, 1503, 4339, 3264, 1376, 607, 3047, 1325, 3020, 4941, 1700, 4964, 1373, 4848, 145, 2441, 3950, 778, 3939, 3183, 3038, 2685, 4939, 4795, 4719, 4126, 1114, 2729, 365, 469, 4945, 3367, 898, 4135, 4687, 2429, 1635, 3414, 456, 171, 3327, 1891, 2827, 48, 4082, 4426, 195, 3849, 1929, 251, 2751, 850, 3745, 4810, 4585, 3029, 327, 3261, 3248, 973, 980, 2461, 72, 4497, 2145, 1240, 2796, 3325, 4487, 2103, 4878, 373, 4218, 1507, 3948, 3690, 3856, 4272, 621, 1366, 4396, 4446, 2675, 4932, 3757, 4900, 3836, 4584, 3131, 242, 4973, 725, 2661, 493, 100, 2021, 735, 1156, 1082, 2596, 3759, 1771, 1310, 1460, 2856, 4285, 853, 3058, 710, 3349, 4216, 1645, 4940, 4614, 4442, 2215, 2876, 1555, 4451, 1129, 109, 1540, 2269, 3088, 1249, 136, 2179, 1598, 3072, 2755, 796, 85, 4709, 238, 2790, 4372, 2227, 3811, 2379, 4400, 4538, 2243, 618, 3604, 2413, 4852, 1543, 3704, 3195, 4988, 3291, 1875, 3960, 4774, 2321, 3262, 1234, 4158, 2750, 732, 3974, 3551, 3049, 1095, 4811, 1858, 822, 2537, 3502, 4846, 295, 2443, 563, 3066, 3018, 1615, 1976, 319, 1383, 3331, 4169, 2022, 4328, 4537, 692, 408, 4970, 1046, 1278, 3219, 612, 670, 2704, 3086, 1908, 70, 2659, 4618, 3354, 3139, 3666, 3128, 4550, 4264, 4008, 600, 1805, 830, 3, 2942, 1417, 628, 4706, 2098, 3982, 4017, 3916, 1441, 522, 112, 2468, 23, 1746, 730, 4992, 1061, 1374, 2571, 267, 1732, 4304, 4997, 4165, 4723, 605, 2562, 873, 3318, 117, 846, 3256, 4630, 1358, 1343, 4711, 4329, 1621, 2336, 2341, 4780, 4598, 4684, 1298, 3517, 1566, 4915, 2373, 4465, 2427, 314, 457, 4745, 2742, 2382, 4065, 3110, 2806, 1385, 2039, 1, 1434, 2741, 787, 1737, 4781, 4441, 3748, 1773, 1079, 4134, 249, 855, 2504, 1208, 4105, 939, 1539, 318, 67, 1592, 409, 3878, 12, 1218, 1557, 1696, 1316, 558, 3691, 1678, 4578, 154, 2101, 900, 3409, 1909, 1659, 643, 4498, 2040, 2134, 4662, 1143, 1783, 483, 1162, 2728, 1710, 2171, 4972, 1110, 3245, 4205, 950, 4171, 1397, 2897, 163, 1292, 3138, 2706, 810, 4274, 4518, 3031, 101, 4313, 4493, 4365, 1879, 1570, 2492, 1658, 4351, 4228, 1835, 3271, 3714, 3383, 1940, 2776, 4680, 3652, 2294, 3182, 7, 1035, 4292, 241, 4499, 4031, 2499, 2458, 226, 2764, 2927, 4188, 1177, 4886, 4255, 2833, 3575, 3764, 2667, 3493, 828, 4245, 1813, 4390, 3443, 1494, 906, 3935, 4787, 4237, 3847, 2846, 4832, 4307, 672, 19, 2348, 1765, 3628, 1150, 4500, 4549, 3033, 2123, 2495, 3127, 55, 2210, 2581, 262, 4756, 2760, 3243, 1977, 2391, 4209, 1317, 4252, 3469, 1364, 679, 4174, 552, 3144, 4490, 2975, 1276, 4956, 4284, 2059, 3166, 3875, 1476, 788, 1704, 3564, 1500, 4710, 2529, 3631, 1350, 2533, 3048, 4262, 1005, 3644, 506, 4286, 3151, 3257, 2475, 3882, 3779, 1454, 451, 1622, 4124, 1295, 383, 367, 4626, 2635, 4777, 1213, 4720, 540, 1625, 1786, 2723, 4007, 1140, 2881, 2813, 1713, 3730, 3967, 1601, 1954, 4327, 3573, 832, 1896, 3260, 1254, 1424, 1568, 1980, 1223, 454, 4926, 4818, 4841, 660, 425, 1930, 2284, 3184, 4606, 1301, 433, 3174, 3479, 2973, 2093, 3649, 1456, 2905, 4142, 831, 1107, 2096, 3884, 3055, 3821, 3464, 4241, 2029, 3170, 2082, 3098, 2000, 2557, 1242, 4675, 2850, 2364, 1451, 3330, 3612, 2780, 3622, 2048, 2452, 3942, 1169, 1703, 2163, 204, 2347, 1439, 776, 3594, 3735, 4591, 3842, 2469, 3050, 1326, 2930, 2252, 4484, 479, 1182, 986, 1405, 1924, 3592, 4159, 3957, 2498, 2064, 2886, 3669, 4259, 1740, 982, 2837, 768, 2512, 1087, 4867, 2593, 726, 3785, 1551, 3344, 2888, 993, 4752, 4463, 4253, 2893, 1430, 546, 1912, 140, 1918, 4517, 3036, 3927, 271, 3489, 2261, 3834, 4163, 2696, 4899, 3329, 3880, 2477, 2415, 3750, 2384, 4457, 3746, 4918, 4280, 4348, 1363, 4053, 2042, 458, 2279, 570, 3978, 4018, 1072, 1486, 954, 3572, 1429, 2266, 1613, 4, 1368, 3500, 2395, 814, 1508, 2712, 22, 142, 635, 1869, 3535, 592, 4472, 3528, 1168, 1979, 3199, 4816, 2889, 3313, 911, 169, 1572, 893, 398, 351, 4844, 478, 4686, 1836, 531, 1673, 4768, 4703, 4289, 2219, 4696, 2914, 1942, 4025, 2592, 3971, 1348, 2587, 412, 4443, 2745, 2467, 413, 1600, 713, 1502, 1815, 717, 3484, 1493, 657, 452, 4773, 2580, 3976, 901, 2882, 1472, 4586, 3099, 3547, 3959, 719, 2177, 170, 4559, 114, 3865, 4403, 1806, 473, 2858, 4985, 2791, 1793, 3667, 3266, 3311, 2549, 3615, 4116, 3874, 193, 4759, 2153, 2500, 3081, 1053, 3158, 4542, 934, 394, 2426, 2577, 2422, 1655, 3742, 1688, 2485, 4991, 3755, 4097, 4511, 3774, 4003, 603, 4783, 4193, 4211, 4732, 1338, 2211, 4314, 2024, 2363, 4011, 4102, 4674, 400, 2747, 2417, 595, 2902, 4409, 2707, 1782, 3852, 1334, 3823, 3585, 1179, 4236, 992, 4576, 4857, 4697, 1739, 658, 56, 470, 4757, 2322, 3156, 4489, 3625, 1238, 2511, 2143, 106, 2462, 2465, 1749, 1185, 4042, 3341, 4746, 4603, 3173, 3305, 131, 4100, 1319, 3456, 3009, 2289, 1788, 2386, 1103, 1760, 3470, 3247, 1478, 198, 3990, 1614, 3675, 4495, 3819, 1404, 1623, 763, 539, 3841, 294, 3716, 3736, 4545, 1483, 545, 4271, 1067, 4571, 3052, 4551, 3214, 300, 4480, 1631, 221, 1816, 728, 1485, 297, 4501, 3482, 1853, 1384, 2068, 3765, 1131, 1520, 3848, 2271, 3289, 2829, 813, 3936, 2666, 1085, 2835, 4479, 4637, 1618, 2329, 3662, 3275, 3035, 4392, 604, 2567, 4628, 2032, 2069, 4829, 1624, 2138, 1435, 2046, 4249, 564, 4556, 1552, 3455, 3851, 4503, 843, 1967, 4529, 1769, 1144, 3215, 1275, 2220, 4326, 1799, 2097, 208, 3096, 4906, 974, 3830, 3083, 4935, 62, 4341, 2761, 2488, 2418, 584, 3353, 1868, 953, 3270, 3754, 1340, 1448, 3458, 4547, 4170, 3588, 3511, 1724, 2406, 3678, 3818, 3532, 2030, 2088, 4825, 3231, 2654, 4152, 904, 1726, 4287, 2290, 1257, 4882, 1590, 1480, 4063, 228, 4604, 467, 4753, 161, 4006, 4876, 167, 1091, 1211, 3747, 714, 668, 152, 3985, 3647, 4118, 2758, 482, 2770, 3555, 514, 183, 4786, 3828, 4196, 4836, 1865, 3310, 2401, 2773, 2142, 3332, 407, 3225, 3395, 5000, 2844, 4504, 4589, 3235, 1804, 3661, 3240, 2670, 2332, 4295, 1562, 196, 2753, 3294, 350, 3356, 3609, 264, 1490, 1332, 26, 4790, 3126, 2258, 784, 2338, 4663, 3286, 1558, 1389, 3324, 3922, 1021, 3080, 673, 2777, 620, 913, 1297, 4093, 1112, 591, 3514, 706, 940, 4476, 3737, 2853, 3835, 3465, 1628, 3340, 1357, 2160, 3829, 3733, 3272, 2646, 440, 49, 2156, 2873, 1863, 3599, 1828, 3146, 2890, 610, 4145, 2420, 1719, 4879, 1086, 503, 4966, 2594, 2541, 1116, 4055, 4343, 2254, 908, 2389, 3448, 2036, 4361, 2280, 3768, 961, 4422, 865, 1054, 1077, 1447, 1058, 4260, 720, 3253, 4364, 1256, 4213, 2011, 1767, 1382, 625, 4420, 3006, 2622, 3925, 4950, 1409, 826, 2929, 1100, 733, 4507, 317, 3390, 1911, 4077, 3122, 664, 20, 2121, 1859, 2451, 671, 3108, 4573, 2154, 2982, 1078, 277, 387, 432, 4140, 1331, 2866, 1861, 4160, 1379, 2356, 2157, 2295, 862, 2814, 287, 4233, 3143, 4305, 3042, 1491, 3358, 2517, 3543, 2932, 2352, 4767, 4854, 991, 2392, 4957, 4933, 2874, 845, 4034, 253, 3206, 3085, 3798, 1965, 1010, 331, 2089, 4332, 3871, 2786, 852, 2043, 3877, 4853, 3421, 4078, 3596, 2339, 2521, 1885, 2149, 525, 3534, 1578, 3314, 1241, 3285, 3361, 2875, 3614, 2826, 1927, 4870, 3486, 3968, 2368, 4425, 4460, 919, 2374, 4373, 724, 1860, 1751, 39, 374, 665, 2200, 2564, 2555, 2012, 968, 188, 4516, 2752, 3161, 3766, 4943, 3345, 3621, 4734, 1801, 338, 4540, 2350, 2206, 1948, 2960, 423, 225, 777, 3027, 4435, 427, 882, 1394, 4155, 827, 4251, 523, 581, 4700, 92, 495, 1042, 4713, 1784, 247, 2787, 4212, 3965, 3869, 2152, 2630, 1996, 2067, 502, 646, 216, 2964, 3607, 4683, 587, 2083, 61, 2715, 888, 1823, 3002, 2860, 513, 825, 4772, 476, 2473, 4871, 185, 1274, 4221, 4620, 1375, 3229, 2797, 2497, 2879, 1781, 1190, 4944, 1227, 837, 1209, 820, 627, 2924, 1775, 1693, 4452, 616, 78, 1262, 3503, 2277, 2976, 650, 515, 2702, 1757, 1113, 3964, 418, 4785, 3630, 2572, 3720, 4750, 4267, 801, 800, 1925, 1797, 1591, 38, 2359, 2128, 2749, 4369, 2800, 2198, 3457, 2614, 1729, 3728, 1668, 1309, 3189, 1933, 2693, 2044, 21, 3201, 1921, 4881, 833, 4979, 3398, 971, 1616, 3117, 4616, 3439, 1741, 2956, 3896, 1237, 2523, 2313, 3381, 1920, 684, 1481, 322, 3629, 4996, 116, 3393, 624, 3011, 909, 3454, 1413, 4755, 4721, 4370, 1133, 3831, 2644, 3857, 1418, 2242, 1492, 4458, 3109, 1495, 4874, 2086, 707, 1571, 4625, 743, 1712, 143, 3546, 1725, 2428, 1158, 2703, 1101, 227, 2167, 4629, 690, 3693, 2435, 1105, 3226, 353, 4962, 4582, 3707, 2779, 4685, 4401, 4359, 1123, 1524, 1467, 989, 79, 1850, 3441, 2246, 835, 1839, 1203, 4658, 3972, 1000, 4012, 1461, 2909, 1682, 1231, 3165, 3104, 4375, 1639, 3908, 71, 265, 371, 916, 4109, 2940, 4567, 66, 4338, 1872, 2969, 4919, 1517, 4983, 716, 615, 3192, 1163, 123, 4666, 1247, 4619, 4821, 2627, 141, 3132, 2629, 749, 4861, 3770, 666, 35, 2884, 3118, 1184, 3283, 1728, 4013, 2225, 3162, 565, 3800, 361, 391, 4379, 2682, 3090, 3583, 3369, 4094, 68, 4040, 1935, 301, 3352, 419, 3605, 2360, 2570, 4917, 2701, 147, 2199, 3763, 2963, 1264, 4788, 3637, 1510, 2781, 258, 27, 417, 4617, 1541, 4302, 1580, 1916, 2109, 1040, 1581, 4727, 2264, 3307, 1128, 1556, 2065, 3290, 985, 2323, 32, 3816, 329, 2345, 4430, 385, 421, 2709, 2663, 1391, 1778, 647, 50, 3864, 1723, 1637, 2906, 927, 1848, 3904, 2824, 638, 2217, 653, 191, 681, 3359, 3076, 2510, 13, 3504, 708, 4046, 3434, 2028, 4062, 1226, 3787, 1626, 2084, 1945, 4560, 1392, 2050, 57, 3726, 1090, 462, 1897, 4079, 2700, 4524, 149, 3660, 1477, 3438, 4308, 840, 501, 1952, 4173, 1753, 2698, 1225, 4946, 4513, 4248, 4386, 3442, 2381, 3706, 812, 4691, 1051, 260, 2892, 2063, 1353, 3121, 2193, 1834, 685, 3278, 3494, 3147, 4357, 88, 2981, 984, 237, 4057, 854, 629, 2434, 2241, 3843, 4801, 559, 3249, 3212, 1175, 640, 3091, 3970, 316, 95, 4530, 4642, 662, 1196, 343, 3597, 4912, 4176, 119, 3767, 1007, 4741, 4887, 3115, 1633, 4291, 680, 746, 1526, 795, 1029, 3548, 2735, 4953, 4636, 2528, 1535, 138, 3795, 924, 1991, 520, 4336, 942, 3590, 4594, 2340, 1453, 3204, 3912, 808, 2515, 4133, 2988, 695, 1662, 3428, 311, 2799, 1155, 701, 186, 970, 2959, 4897, 1734, 3664, 2155, 1296, 107, 2158, 4994, 4989, 1041, 348, 2556, 194, 94, 3632, 2928, 1519, 2934, 4577, 3487, 791, 4399, 4884, 2074, 406, 1692, 2459, 120, 3475, 4384, 82, 1528, 1307, 2438, 384, 4894, 2445, 2757, 1370, 5, 3509, 2774, 4033, 2733, 3444, 948, 2672, 1931, 3756, 1735, 4204, 775, 2632, 1074, 399, 1522, 2159, 4146, 1981, 4522, 1561, 3979, 29, 337, 1855, 912, 2330, 3413, 1445, 1130, 4978, 1594, 4512, 1028, 2788, 1512, 1064, 202, 4793, 2830, 2421, 3363, 798, 3680, 3149, 2472, 3388, 2609, 3758, 1165, 3977, 3319, 2049, 3549, 4256, 4922, 1674, 2091, 781, 2613, 2546, 4566, 1649, 2409, 3185, 1504, 892, 1575, 1579, 3429, 3749, 302, 3949, 817, 3188, 1856, 3741, 4707, 2623, 288, 4035, 3232, 2436, 47, 1324, 4041, 2311, 3114, 4792, 2544, 3472, 567, 3450, 3657, 2607, 3377, 981, 2878, 305, 1536, 1887, 4148, 2920, 823, 1677, 1117, 1657, 1457, 4986, 683, 4294, 2094, 156, 3308, 553, 3560, 1906, 3784, 3624, 2883, 2403, 599, 941, 4297, 3814, 118, 1127, 4029, 594, 1582, 3375, 2135, 10, 1137, 2524, 3627, 4161, 1585, 4488, 2520, 2328, 2168, 3224, 1057, 1075, 644, 548, 1043, 441, 1715, 4238, 4290, 4977, 2317, 1268, 1222, 3054, 589, 2071, 3999, 2563, 1810, 634, 1627, 3558, 3430, 466, 135, 3899, 160, 1718, 269, 2922, 453, 3391, 542, 4043, 3480, 2917, 376, 2251, 174, 1084, 340, 555, 1854, 1612, 3133, 2608, 4113, 4914, 2694, 4778, 4623, 3719, 4600, 2127, 3102, 3357, 4449, 2484, 4823, 1221, 3868, 3394, 4014, 2388, 3154, 1951, 4682, 958, 33, 3032, 4819, 3683, 1198, 2203, 1547, 3190, 508, 2125, 4119, 3254, 920, 2077, 3832, 1411, 4862, 4201, 4180, 3954, 1886, 1407, 4151, 3267, 2772, 675, 1132, 1151, 4413, 4901, 3589, 2062, 219, 4820, 3928, 2423, 4257, 4775, 1327, 3023, 1427, 4202, 1684, 2650, 2628, 3531, 1006, 649, 3699, 1830, 507, 113, 932, 3499, 1795, 396, 1045, 386, 651, 1974, 983, 4667, 4814, 4640, 3427, 3155, 4561, 4206, 2385, 2442, 2531, 3526, 4239, 3633, 2912, 4779, 45, 1663, 1487, 2565, 4763, 4845, 3094, 1780, 3577, 1056, 3284, 2174, 4207, 420, 4172, 2239, 652, 3783, 2190, 931, 1802, 2218, 535, 3476, 856, 836, 3399, 3010, 2222, 632, 3481, 3176, 3410, 857, 1261, 64, 1553, 307, 3276, 4595, 372, 4747, 126, 3981, 3650, 3371, 278, 1423, 4948, 1857, 3740, 283, 91, 4432, 648, 517, 1497, 4312, 1542, 1197, 4913, 1081, 4712, 1846, 4641, 889, 3953, 2259, 3172, 2175, 3791, 2896, 3905, 3420, 4740, 4868, 2782, 215, 4358, 2859, 4483, 3288, 2611, 3540, 139, 3697, 1333, 3580, 4758, 1838, 4771, 611, 176, 3022, 4708, 15, 3705, 2660, 571, 3426, 3207, 3645, 98, 4389, 3100, 2501, 2045, 3452, 1352, 2737, 4635, 1537, 1790, 1953, 4346, 2645, 1791, 3854, 2312, 2961, 874, 1505, 1252, 1584, 1293, 2002, 497, 3679, 4809, 2140, 2278, 4164, 3626, 4782, 4908, 2346, 3941, 1923, 2378, 3909, 1415, 4311, 2977, 4817, 3280, 146, 4689, 3616, 3242, 4544, 3082, 3659, 2677, 4947, 1957, 4456, 1994, 2191, 3101, 700, 4030, 2998, 2602, 999, 1905, 132, 14, 1425, 761, 536, 2025, 4486, 661, 4467, 3673, 1285, 4141, 3618, 2789, 4197, 180, 4574, 699, 1634, 1436, 1513, 2035, 2516, 1442, 841, 3986, 2910, 1762, 2058, 663, 3751, 212, 978, 1164, 1037, 3024, 596, 366, 1378, 4345, 3919, 375, 4166, 2640, 3322, 4934, 4519, 3299, 588, 3385, 1559, 443, 4101, 1532, 431, 2604, 4535, 304, 500, 802, 1126, 844, 4650, 3715, 1329, 1632, 447, 4644, 2054, 2731, 4132, 1216, 1406, 1428, 2727, 2017, 4123, 2792, 2944, 4990, 3433, 2394, 2714, 1388, 4191, 3709, 4693, 1651, 1599, 2583, 234, 1707, 696, 2999, 4015, 1059, 3773, 3717, 4412, 705, 1899, 74, 2178, 2237, 2176, 4153, 1484, 1533, 2952, 4833, 3996, 3885, 1471, 4835, 2234, 4520, 3005, 496, 11, 2986, 4593, 2641, 4744, 1199, 25, 2578, 87];

n = 5000

def bubble_sort():
    
    # Traverse through all array elements
    for i in range(n):
        swapped = False

        # Last i elements are already in place
        for j in range(0, n - i - 1):
            # traverse the array from 0 to n-i-1
            # Swap if the element
            # found is greater than the
            # next element
            if arr[j] > arr[j + 1]:
                arr[j], arr[j + 1] = arr[j + 1], arr[j]
                swapped = True

        # IF no two elements were swapped
        # by inner loop, then break
        if not swapped:
            break


# Driver code to test above
if __name__ == '__main__':
    bubble_sort()