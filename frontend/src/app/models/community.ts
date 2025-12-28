// =====================================================
// COMMUNITY SOCIAL MODELS
// =====================================================

// ============== ENUMS ==============

export enum LoaiBaiViet {
  THAO_LUAN = 'THAO_LUAN',
  CAU_HOI = 'CAU_HOI',
  CHIA_SE = 'CHIA_SE',
  HUONG_DAN = 'HUONG_DAN',
  THONG_BAO = 'THONG_BAO',
}

export enum TrangThaiBaiViet {
  CHO_DUYET = 'CHO_DUYET',
  DA_DUYET = 'DA_DUYET',
  AN = 'AN',
  TU_CHOI = 'TU_CHOI',
  DA_XOA = 'DA_XOA',
}

export enum LoaiBaoCao {
  SPAM = 'SPAM',
  NSFW = 'NSFW',
  HARASSMENT = 'HARASSMENT',
  MISINFORMATION = 'MISINFORMATION',
  COPYRIGHT = 'COPYRIGHT',
  OTHER = 'OTHER',
}

export enum TrangThaiBaoCao {
  CHO_XU_LY = 'CHO_XU_LY',
  DA_XU_LY = 'DA_XU_LY',
  TU_CHOI = 'TU_CHOI',
}

// ============== INTERFACES ==============

export interface Tag {
  id: number;
  ten: string;
  slug: string;
  moTa?: string;
  mauSac: string;
  icon?: string;
  soBaiViet: number;
  thuTu: number;
  hienThi: boolean;
}

export interface NguoiDangResponse {
  id: number;
  ten: string;
  anhDaiDien?: string;
  capDo: number;
}

export interface HinhAnhBaiViet {
  id: number;
  duongDan: string;
  moTa?: string;
  thuTu: number;
}

export interface BaiViet {
  id: number;
  nguoiDang?: NguoiDangResponse;
  tieuDe: string;
  noiDung: string;
  loai: string;
  trangThai: string;
  luotXem: number;
  soLuotThich: number;
  soLuotBinhLuan: number;
  ghim: boolean;
  tags?: Tag[];
  hinhAnh?: HinhAnhBaiViet[];
  daThich: boolean;
  daLuu: boolean;
  laCuaToi: boolean;
  daSua: boolean;
  ngayTao: string;
  ngayCapNhat: string;
}

export interface BinhLuan {
  id: number;
  nguoiDang?: NguoiDangResponse;
  noiDung: string;
  soLuotThich: number;
  daThich: boolean;
  biAn: boolean;
  daSua: boolean;
  laCuaToi: boolean;
  binhLuanCon?: BinhLuan[];
  ngayTao: string;
  ngayCapNhat: string;
}

export interface BaoCao {
  id: number;
  baiViet?: { id: number; tieuDe: string };
  binhLuan?: { id: number };
  nguoiBaoCao?: NguoiDangResponse;
  loai: string;
  chiTiet?: string;
  trangThai: string;
  nguoiXuLy?: NguoiDangResponse;
  ghiChuXuLy?: string;
  ngayTao: string;
  ngayXuLy?: string;
}

// ============== DTOs ==============

export interface BaiVietDTO {
  tieuDe: string;
  noiDung: string;
  loai?: LoaiBaiViet;
  tagIds?: number[];
  hinhAnhUrls?: string[];
}

export interface BinhLuanDTO {
  baiVietId: number;
  noiDung: string;
  binhLuanChaId?: number;
}

export interface BaoCaoDTO {
  loai: LoaiBaoCao;
  chiTiet?: string;
  baiVietId?: number;
  binhLuanId?: number;
}

export interface XuLyBaoCaoDTO {
  chapNhan: boolean;
  ghiChu?: string;
}

// Aliases for service compatibility
export type CreateBaiVietDTO = BaiVietDTO;
export type CreateBinhLuanDTO = BinhLuanDTO;
export type CreateBaoCaoDTO = BaoCaoDTO;

// ============== PAGE RESPONSE ==============

export interface PageResponse<T> {
  items: T[];
  currentPage: number;
  totalPages: number;
  totalItems: number;
  pageSize: number;
}

// ============== API RESPONSE ==============

export interface ApiResponse<T> {
  status: string;
  message: string;
  data: T;
}

// ============== LOẠI BÀI VIẾT OPTIONS ==============

export const LOAI_BAI_VIET_OPTIONS = [
  { value: LoaiBaiViet.THAO_LUAN, label: 'Thảo luận', icon: '💬', color: '#6366f1' },
  { value: LoaiBaiViet.CAU_HOI, label: 'Câu hỏi', icon: '❓', color: '#f59e0b' },
  { value: LoaiBaiViet.CHIA_SE, label: 'Chia sẻ', icon: '📤', color: '#10b981' },
  { value: LoaiBaiViet.HUONG_DAN, label: 'Hướng dẫn', icon: '📚', color: '#3b82f6' },
  { value: LoaiBaiViet.THONG_BAO, label: 'Thông báo', icon: '📢', color: '#ef4444' },
];

export const LOAI_BAO_CAO_OPTIONS = [
  { value: LoaiBaoCao.SPAM, label: 'Spam', icon: '🚫' },
  { value: LoaiBaoCao.NSFW, label: 'Nội dung không phù hợp', icon: '🔞' },
  { value: LoaiBaoCao.HARASSMENT, label: 'Quấy rối', icon: '😡' },
  { value: LoaiBaoCao.MISINFORMATION, label: 'Thông tin sai lệch', icon: '📰' },
  { value: LoaiBaoCao.COPYRIGHT, label: 'Vi phạm bản quyền', icon: '©️' },
  { value: LoaiBaoCao.OTHER, label: 'Khác', icon: '📝' },
];
