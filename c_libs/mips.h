/*
    Provides an interface for direct replacement of the following operations:
    
    sin
    cos
    tan
    acos
    asin
    atan
    atan2

    abs
    ceil
    floor
    trunc
    round

    max
    min

    exp
    log
    sqrt

    rand

    sleep
    yield
*/

typedef enum {
    DEVICE_0,
    DEVICE_1,
    DEVICE_2,
    DEVICE_3,
    DEVICE_4,
    DEVICE_5
} device_id_t ;

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
    VOLUME
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
    REFERENCE_ID
} slot_variable_t;

typedef enum {
    CONTENTS,
    REQUIRED,
    RECIPE
} reagent_mode_t;

typedef enum {
    AVERAGE,
    SUM,
    MINIMUM,
    MAXIMUM,
} batch_mode_t;

typedef int item_hash_t;

#pragma DIRECT_REPLACED_FUNCTIONS
float m_sin(float a);
float m_cos(float a);
float m_tan(float a);
float m_asin(float a);
float m_acos(float a);
float m_atan(float a);
float m_atan2(float a, float b);
float m_abs(float a);
float m_ceil(float a);
float m_floor(float a);  
float m_trunc(float a);  
float m_round(float a);
float m_min(float a, float b);
float m_max(float a, float b);
float m_exp(float a);
float m_log(float a);
float m_sqrt(float a);
float m_rand(float a);

void m_sleep(float a);
void m_yeild();

item_hash_t HASH(char* a) {}
void load(float* dest, device_id_t device, device_variable_t device_var) {}
void load_reagent(float* dest, device_id_t device, reagent_mode_t reagent_mode, item_hash_t reagent_type) {}
void load_slot(float* dest, device_id_t device, int slot_index, slot_variable_t slot_var) {}
void load_batch(float* dest, item_hash_t device_type, device_variable_t device_var, batch_mode_t batch_mode) {}
void load_batch_slot(float* dest, item_hash_t device_type, int slot_index, slot_variable_t device_var, batch_mode_t batch_mode) {}
void load_batch_with_name(float* dest, item_hash_t device_type, char* device_name, device_variable_t device_var, batch_mode_t batch_mode) {}
void load_batch_with_name_slot(float* dest, item_hash_t device_type, char* device_name, int slot_index, slot_variable_t device_var, batch_mode_t batch_mode) {}
void store(device_id_t device, device_variable_t device_var, float source) {}
void store_slot(device_id_t device, int slot_index, slot_variable_t slot_var, float source) {}
void store_batch(item_hash_t device_type, device_variable_t device_var, float source) {}
void store_batch_with_name(item_hash_t device_type, char* device_name, device_variable_t device_var, float source) {}
void store_batch_slot(item_hash_t device_type, int slot_index, slot_variable_t device_var, float source) {}

