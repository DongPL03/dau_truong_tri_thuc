export interface PhanTichHocTapResponse {
  id: number;
  khoa_hoc_id: number;
  khoa_hoc_ten: string;
  diem_manh: string[];
  diem_yeu: string[];
  chu_de_manh: ChuDeInfo[];
  chu_de_yeu: ChuDeInfo[];
  giai_phap: string;
  cap_nhat_luc: string;
}

export interface ChuDeInfo {
  id: number;
  ten: string;
  ti_le_dung: number;
}


