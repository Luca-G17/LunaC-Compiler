#include "game_types.h"

typedef int item_hash_t;
void load(float* dest, device_id_t device, device_variable_t device_var) {}
void load_reagent(float* dest, device_id_t device, reagent_mode_t reagent_mode, int reagent_hash) {}
void load_slot(float* dest, device_id_t device, int slot_index, slot_variable_t slot_var) {}
void load_batch(float* dest, item_hash_t device_type, device_variable_t device_var, batch_mode_t batch_mode) {}
void load_batch_slot(float* dest, item_hash_t device_type, int slot_index, slot_variable_t slot_var, batch_mode_t batch_mode) {}
void load_batch_with_name(float* dest, item_hash_t device_type, char* device_name, device_variable_t device_var, batch_mode_t batch_mode) {}
void load_batch_with_name_slot(float* dest, item_hash_t device_type, char* device_name, int slot_index, slot_variable_t slot_var, batch_mode_t batch_mode) {}
void store(device_id_t device, device_variable_t device_var, float source) {}
void store_slot(device_id_t device, int slot_index, slot_variable_t slot_var, float source) {}
void store_batch(item_hash_t device_type, device_variable_t device_var, float source) {}
void store_batch_with_name(item_hash_t device_type, char* device_name, device_variable_t device_var, float source) {}
void store_batch_slot(item_hash_t device_type, int slot_index, slot_variable_t slot_var, float source) {}
float m_sin(float __x);
float m_cos(float __x);
float m_tan(float __x);
float m_asin(float __x);
float m_acos(float __x);
float m_atan(float __x);
float m_atan2(float __x, float __y);
float m_abs(float __x);
float m_ceil(float __x);
float m_floor(float __x);
float m_trunc(float __x);
float m_round(float __x);
float m_min(float __x, float __y);
float m_max(float __x, float __y);
float m_exp(float __x);
float m_log(float __x);
float m_sqrt(float __x);
float m_rand(float __x);
void m_sleep(float __x);
void m_yeild();
