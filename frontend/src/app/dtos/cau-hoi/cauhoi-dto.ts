export class CauHoiDTO {
  bo_cau_hoi_id!: number;
  noi_dung!: string;
  do_kho: 'DE' | 'TRUNG_BINH' | 'KHO' = 'TRUNG_BINH';
  loai_noi_dung: 'VAN_BAN' | 'HINH_ANH' | 'AM_THANH' | 'VIDEO' = 'VAN_BAN';
  duong_dan_tep?: string | null;
  lua_chon_a!: string;
  lua_chon_b!: string;
  lua_chon_c!: string;
  lua_chon_d!: string;
  dap_an_dung!: 'A' | 'B' | 'C' | 'D';
  giai_thich?: string;

  [key: string]: any;

  constructor(data?: Partial<CauHoiDTO>) {
    if (data) {
      Object.assign(this, data);
    }
  }
}
