// src/app/responses/practice/submit-luyen-tap-response.ts
export interface SubmitLuyenTapAnswerResult {
  cau_hoi_id: number;
  lua_chon: 'A' | 'B' | 'C' | 'D';
  dung_hay_sai: boolean;
  dap_an_dung: string | null;
  giai_thich: string | null;
  thoi_gian_ms?: number | null;
}

export interface SubmitLuyenTapResponse {
  phien_id: number;
  tong_so_cau: number;
  so_cau_dung: number;
  diem_so: number;
  do_chinh_xac: number; // BigDecimal -> number
  thoi_gian_tb_ms: number;
  chi_tiet: SubmitLuyenTapAnswerResult[];
}
