/**
 * Model cho kho đồ (inventory)
 */
export interface KhoDoResponse {
  tong_loai_vat_pham: number;
  tong_so_luong: number;
  danh_sach_vat_pham: InventoryItem[];
}

export interface InventoryItem {
  id: number;
  vat_pham_id: number;
  ten: string;
  mo_ta: string;
  icon: string;
  loai: string;
  do_hiem: string;
  so_luong: number;
  nhan_luc: string | null;
  su_dung_luc: string | null;
  co_the_su_dung: boolean;
  hieu_ung: string;
}
