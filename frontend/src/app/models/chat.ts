// ============== ENUMS ==============

export enum LoaiPhongChat {
  DON = 'DON',
  NHOM = 'NHOM',
}

export enum LoaiTinNhan {
  VAN_BAN = 'VAN_BAN',
  HINH_ANH = 'HINH_ANH',
  TAP_TIN = 'TAP_TIN',
  AM_THANH = 'AM_THANH',
  HE_THONG = 'HE_THONG',
  STICKER = 'STICKER',
  EMOJI = 'EMOJI',
}

export enum VaiTroPhongChat {
  ADMIN = 'ADMIN',
  THANH_VIEN = 'THANH_VIEN',
}

// ============== INTERFACES ==============

export interface NguoiGui {
  id: number;
  ten: string;
  anhDaiDien?: string;
}

export interface NguoiChat {
  id: number;
  ten: string;
  anhDaiDien?: string;
  online?: boolean;
}

export interface ThanhVienPhongChat {
  id: number;
  nguoiDungId: number;
  ten: string;
  anhDaiDien?: string;
  bietDanh?: string;
  vaiTro: VaiTroPhongChat | string;
  thamGiaLuc: string;
  online?: boolean;
}

export interface TinNhanTraLoi {
  id: number;
  noiDung: string;
  tenNguoiGui: string;
}

export interface TinNhan {
  id: number;
  phongChatId: number;
  nguoiGui: NguoiGui;
  loai: LoaiTinNhan;
  noiDung?: string;
  urlMedia?: string;
  tenFile?: string;
  kichThuocFile?: number;
  traLoiCho?: TinNhanTraLoi;
  guiLuc: string;
  chinhSuaLuc?: string;
  daGhim?: boolean;
  laToi?: boolean;
}

export interface PhongChat {
  id: number;
  ten?: string;
  anhNhom?: string;
  loai: LoaiPhongChat;
  taoLuc: string;
  capNhatLuc: string;
  tinNhanCuoi?: string;
  thoiGianTinNhanCuoi?: string;
  thanhVien: ThanhVienPhongChat[];
  soTinNhanChuaDoc: number;
  daGhim?: boolean;
  daTatThongBao?: boolean;
  nguoiChat?: NguoiChat; // For 1-1 chat
}

// ============== DTOs ==============

export interface TaoPhongChatDTO {
  ten?: string;
  thanhVienIds: number[];
  anhNhom?: string;
}

export interface GuiTinNhanDTO {
  phongChatId: number;
  noiDung?: string;
  loai?: LoaiTinNhan;
  urlMedia?: string;
  tenFile?: string;
  kichThuocFile?: number;
  traLoiChoId?: number;
}

export interface CapNhatPhongChatDTO {
  ten?: string;
  anhNhom?: string;
  themThanhVien?: number[];
  xoaThanhVien?: number[];
}

// ============== API RESPONSE ==============

export interface ChatApiResponse<T> {
  status: string;
  message: string;
  data?: T;
}

export interface ChatPageResponse<T> {
  items: T[];
  totalPages: number;
  totalItems: number;
  currentPage: number;
}

// ============== WEBSOCKET ==============

export interface ChatWebSocketMessage {
  type:
    | 'NEW_MESSAGE'
    | 'USER_READ'
    | 'USER_TYPING'
    | 'USER_STOP_TYPING'
    | 'USER_STATUS'
    | 'NEW_CHAT_MESSAGE';
  data: any;
}

// ============== UI STATE ==============

export interface TypingUser {
  userId: number;
  userName: string;
}

export interface ChatState {
  activeRoomId: number | null;
  isLoading: boolean;
  isSending: boolean;
  typingUsers: TypingUser[];
  replyingTo: TinNhan | null;
}
