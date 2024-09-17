typedef enum {
    DEVICE_0,
    DEVICE_1,
    DEVICE_2,
    DEVICE_3,
    DEVICE_4,
    DEVICE_5,
} device_id_t;

typedef enum {
    ACTIVATE,
    AIR_RELEASE,
    CHARGE,
    CLEAR_MEMORY,
    COLOR,
    COMPLETION_RATIO,
    EVELATOR_SPEED,
    ERROR,
    EXPORT_COUNT,
    FILTRATION,
    HARVEST,
    HORIZONTAL,
    HORIZONTAL_RATIO,
    IDLE,
    IMPORT_COUNT,
    LOCK,
    MAXIMUM,
    MODE,
    ON,
    OPEN,
    OUTPUT,
    PLANT,
    POSITION_X,
    POSITION_Y,
    POSITION_Z,
    POWER,
    POWER_ACTUAL,
    POWER_POTENTIAL,
    POWER_REQUIRED,
    PRESSURE,
    PRESSURE_EXTERNAL,
    PRESSURE_SETTING,
    QUANTITY,
    RATIO,
    RATIO_CARBON_DIOXIDE,
    RATIO_NITROGEN,
    RATIO_POLLUTANT,
    RATIO_VOLATILES,
    RATIO_WATER,
    REAGENTS,
    RECIPE_HASH,
    REFERENCE_ID,
    REQUEST_HASH,
    REQUIRED_POWER,
    SETTING,
    SOLAR_ANGLE,
    TEMPERATURE,
    TEMPERATURE_SETTINGS,
    TOTAL_MOLES,
    VELOCITY_MAGNITUDE,
    VELOCITY_RELATIVE_X,
    VELOCITY_RELATIVE_Y,
    VELOCITY_RELATIVE_Z,
    VERTICAL,
    VERTICAL_RATIO,
    VOLUME,
} device_variable_t;

typedef enum {
    OCCUPIED,
    OCCUPANT_HASH,
    QUANTITY,
    DAMAGE,
    EFFICIENCY,
    HEALTH,
    GROWTH,
    PRESSURE,
    TEMPERATURE,
    CHARGE,
    CHARGE_RATIO,
    CLASS,
    PRESSURE_WASTE,
    PRESSURE_AIR,
    MAX_QUANTITY,
    MATURE,
    REFERENCE_ID,
} slot_variable_t;

typedef enum {
    CONTENTS,
    REQUIRED,
    RECIPE,
} reagent_mode_t;

typedef enum {
    AVERAGE,
    SUM,
    MINIMUM,
    MAXIMUM,
} batch_mode_t;
