require(magic)
# Create YearMonth field to be used as a quick lookup for data ranges
magicSQL("UPDATE Friendships_Directional_Final SET YearMonth = concat(cast(Year as char), cast(LPAD(Month, 2, '0') as char))", "cpw_meta")