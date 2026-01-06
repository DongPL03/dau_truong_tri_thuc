
import { CommonModule } from '@angular/common';
import { Component, ElementRef, OnDestroy, OnInit, signal, ViewChild } from '@angular/core';
import { FormsModule } from '@angular/forms';
import { PickerComponent } from '@ctrl/ngx-emoji-mart';
import { Subscription } from 'rxjs';
import { SendMessageDto } from '../../../dtos/tinnhan/send-message-dto';
import {
  GuiTinNhanDTO,
  LoaiPhongChat,
  LoaiTinNhan,
  PhongChat,
  TinNhan,
} from '../../../models/chat';
import { ChatFriendItemResponse } from '../../../responses/chat/chat-friend-item-response';
import { ChatMessageResponse } from '../../../responses/chat/chat-message-response';
import { PageResponse } from '../../../responses/page-response';
import { ResponseObject } from '../../../responses/response-object';
import { Base } from '../../base/base';

// Kiểu chat hiện tại
type ChatTab = 'friends' | 'groups';
type ActiveChatType = 'friend' | 'group';

@Component({
  selector: 'app-chat-main',
  standalone: true,
  imports: [CommonModule, FormsModule, PickerComponent],
  templateUrl: './chat-main.html',
  styleUrl: './chat-main.scss',
})
export class ChatMainComponent extends Base implements OnInit, OnDestroy {
  // Tab hiện tại
  activeTab: ChatTab = 'friends';
  activeChatType: ActiveChatType | null = null;

  // === FRIENDS CHAT ===
  inbox_items: ChatFriendItemResponse[] = [];
  inbox_loading = false;

  messages: ChatMessageResponse[] = [];
  messages_loading = false;
  messages_page = 0;
  messages_limit = 30;
  messages_has_more = true;

  active_partner: ChatFriendItemResponse | null = null;
  new_message_noi_dung = '';
  sending = false;
  meName = '';
  showEmojiPicker = signal<boolean>(false);

  // === GROUP CHAT ===
  groups: PhongChat[] = [];
  groups_loading = false;
  groups_page = 0;
  groups_has_more = true;

  activeGroup: PhongChat | null = null;
  groupMessages: TinNhan[] = [];
  groupMessages_loading = false;
  groupMessages_page = 0;
  groupMessages_has_more = true;

  // Create group modal
  showCreateGroupModal = false;
  newGroupName = '';
  selectedMemberIds: number[] = [];

  // Group settings panel
  showGroupSettings = false;
  editGroupName = '';
  isEditingGroupName = false;

  // Add members modal
  showAddMembersModal = false;
  addMemberSelectedIds: number[] = [];

  // File upload for messages
  selectedFile: File | null = null;
  uploadingFile = false;
  @ViewChild('fileInput') fileInput?: ElementRef<HTMLInputElement>;

  @ViewChild('messagesContainer') messages_container?: ElementRef<HTMLDivElement>;
  @ViewChild('chatInput') chatInput?: ElementRef<HTMLTextAreaElement>;

  private route_partner_id: number | null = null;
  private wsSub?: Subscription;
  private groupWsSub?: Subscription;
  current_user_id: number | null = null;

  LoaiPhongChat = LoaiPhongChat;
  LoaiTinNhan = LoaiTinNhan;

  ngOnInit(): void {
    const currentUser = this.userService.currentUser();
    this.current_user_id = currentUser?.id ?? null;
    this.meName = currentUser?.ho_ten || 'Bạn';

    if (this.current_user_id) {
      this.chatWsService.connect(this.current_user_id);
      this.wsSub = this.chatWsService.messages$.subscribe((msg) => this.handleWsMessage(msg));

      // Connect group chat WebSocket
      this.groupChatService.connect();
      this.groupWsSub = this.groupChatService.messages$.subscribe((msg) =>
        this.handleGroupWsMessage(msg)
      );
    }

    const param = this.route.snapshot.paramMap.get('partner_id');
    this.route_partner_id = param ? +param : null;

    this.load_friends();
    this.load_groups();
  }

  ngOnDestroy(): void {
    this.wsSub?.unsubscribe();
    this.groupWsSub?.unsubscribe();
    this.chatWsService.disconnect();
    this.groupChatService.disconnect();
  }

  // === TAB SWITCHING ===
  switchTab(tab: ChatTab): void {
    this.activeTab = tab;
    // Reset active selections when switching tabs
    if (tab === 'friends') {
      this.activeGroup = null;
      this.activeChatType = this.active_partner ? 'friend' : null;
    } else {
      this.active_partner = null;
      this.activeChatType = this.activeGroup ? 'group' : null;
    }
  }

  // --- 1. Load Inbox ---
  load_friends(): void {
    if (this.inbox_loading) return;
    this.inbox_loading = true;

    this.friendService.getFriends().subscribe({
      next: (res) => {
        const friends = res.data || [];
        this.inbox_items = friends.map((f: any) => ({
          partner_id: f.user_id ?? f.userId,
          partner_name: f.ho_ten ?? f.hoTen,
          partner_avatar_url: f.avatar_url ?? f.avatarUrl,
          partner_status: f.trang_thai ?? 'OFFLINE',
          last_message: null,
          last_time: null,
          unread_count: 0,
        }));
        this.inbox_loading = false;

        if (this.route_partner_id && !this.active_partner) {
          const found = this.inbox_items.find((it) => it.partner_id === this.route_partner_id);
          if (found) this.select_partner(found);
        }
      },
      error: () => (this.inbox_loading = false),
    });
  }

  select_partner(item: ChatFriendItemResponse): void {
    if (this.active_partner?.partner_id === item.partner_id) return;

    this.active_partner = item;
    this.activeChatType = 'friend'; // Set active chat type
    this.activeGroup = null; // Clear group selection
    this.messages = [];
    this.messages_page = 0;
    this.messages_has_more = true;
    item.unread_count = 0;

    this.load_messages(0);
  }

  load_messages(page: number): void {
    if (!this.active_partner || this.messages_loading) return;
    this.messages_loading = true;

    this.chatService
      .getConversation(this.active_partner.partner_id, page, this.messages_limit)
      .subscribe({
        next: (res: ResponseObject<PageResponse<ChatMessageResponse>>) => {
          const page_res = res.data;
          if (page_res) {
            this.messages_page = page_res.currentPage;
            this.messages_has_more = page_res.currentPage + 1 < page_res.totalPages;
            const items = (page_res.items || []).slice().reverse();

            if (page === 0) {
              this.messages = items;
              setTimeout(() => this.scroll_to_bottom(), 100);
            } else {
              this.messages = [...items, ...this.messages];
            }
          }
          this.messages_loading = false;
        },
        error: () => (this.messages_loading = false),
      });
  }

  load_more_messages(): void {
    if (this.messages_has_more && !this.messages_loading) {
      this.load_messages(this.messages_page + 1);
    }
  }

  // --- 3. Send Message ---
  send_message(): void {
    if (!this.active_partner) return;
    const content = this.new_message_noi_dung.trim();
    if (!content || this.sending) return;

    this.sending = true;
    const dto = new SendMessageDto();
    dto.receiver_id = this.active_partner.partner_id;
    dto.noi_dung = content;

    this.chatService.sendMessage(dto).subscribe({
      next: () => {
        this.new_message_noi_dung = '';
        this.sending = false;
        this.showEmojiPicker.set(false); // Đóng emoji khi gửi
        // Scroll bottom tạm thời
        if (this.chatInput?.nativeElement) {
          this.chatInput.nativeElement.style.height = 'auto';
        }
      },
      error: () => (this.sending = false),
    });
  }

  on_enter_send(event: Event): void {
    const kEvent = event as KeyboardEvent;
    if (!kEvent.shiftKey) {
      kEvent.preventDefault();
      this.send_message();
    }
  }

  // --- 4. WebSocket Handle ---
  private handleWsMessage(msg: ChatMessageResponse): void {
    if (!msg || !this.current_user_id) return;

    const meId = this.current_user_id;
    const partnerId = msg.gui_boi_id === meId ? msg.nhan_boi_id : msg.gui_boi_id;

    // Update Inbox List
    const idx = this.inbox_items.findIndex((it) => it.partner_id === partnerId);
    if (idx >= 0) {
      const item = this.inbox_items[idx];
      const isCurrentOpened = this.active_partner && this.active_partner.partner_id === partnerId;

      const updated: ChatFriendItemResponse = {
        ...item,
        last_message: msg.noi_dung,
        last_time: msg.gui_luc,
        unread_count: isCurrentOpened ? 0 : (item.unread_count ?? 0) + 1,
      };

      this.inbox_items.splice(idx, 1);
      this.inbox_items.unshift(updated); // Move to top

      if (isCurrentOpened) this.active_partner = updated;
    }

    // Append Message if Chat Open
    if (this.active_partner && this.active_partner.partner_id === partnerId) {
      this.messages.push(msg);
      setTimeout(() => this.scroll_to_bottom(), 100);
    }
  }

  private scroll_to_bottom(): void {
    if (this.messages_container) {
      const el = this.messages_container.nativeElement;
      el.scrollTo({ top: el.scrollHeight, behavior: 'smooth' });
    }
  }

  // Helper Avatar
  // --- Helper Avatar ---
  // Thêm 'null' vào kiểu dữ liệu của tham số url
  getAvatar(url: string | null | undefined, name: string): string {
    if (url) {
      return `http://localhost:8088/api/v1/users/profile-images/${url}`;
    }
    // Fallback nếu url là null, undefined hoặc rỗng
    return `https://ui-avatars.com/api/?name=${name}&background=random&color=fff&size=128`;
  }

  // --- LOGIC EMOJI ---
  toggleEmojiPicker() {
    this.showEmojiPicker.update((v) => !v);
  }

  addEmoji(event: any) {
    const emoji = event.emoji.native;
    this.new_message_noi_dung += emoji;
    // Sau khi thêm emoji thì gọi resize để ô nhập không bị che
    setTimeout(() => this.autoResize(), 0);
  }

  // --- LOGIC AUTO RESIZE TEXTAREA ---
  autoResize(): void {
    const textarea = this.chatInput?.nativeElement;
    if (textarea) {
      textarea.style.height = 'auto'; // Reset chiều cao để tính toán lại
      textarea.style.height = textarea.scrollHeight + 'px'; // Gán chiều cao theo nội dung
    }
  }

  // ============== GROUP CHAT METHODS ==============

  load_groups(): void {
    if (this.groups_loading) return;
    this.groups_loading = true;

    this.groupChatService.getRooms(this.groups_page, 20).subscribe({
      next: (res) => {
        if (res.data) {
          if (this.groups_page === 0) {
            this.groups = res.data.items || [];
          } else {
            this.groups = [...this.groups, ...(res.data.items || [])];
          }
          this.groups_has_more = (res.data.items?.length || 0) === 20;
        }
        this.groups_loading = false;
      },
      error: () => (this.groups_loading = false),
    });
  }

  load_more_groups(): void {
    if (this.groups_has_more && !this.groups_loading) {
      this.groups_page++;
      this.load_groups();
    }
  }

  select_group(group: PhongChat): void {
    if (this.activeGroup?.id === group.id) return;

    // Unsubscribe from previous room
    if (this.activeGroup) {
      this.groupChatService.unsubscribeFromRoom(this.activeGroup.id);
    }

    this.activeGroup = group;
    this.activeChatType = 'group';
    this.active_partner = null;
    this.groupMessages = [];
    this.groupMessages_page = 0;
    this.groupMessages_has_more = true;

    // Subscribe to new room
    this.groupChatService.subscribeToRoom(group.id);
    this.load_group_messages(0);
    this.mark_group_as_read(group.id);
  }

  load_group_messages(page: number): void {
    if (!this.activeGroup || this.groupMessages_loading) return;
    this.groupMessages_loading = true;

    this.groupChatService.getMessages(this.activeGroup.id, page, 30).subscribe({
      next: (res) => {
        if (res.data) {
          const items = (res.data.items || []).slice().reverse();
          this.groupMessages_page = res.data.currentPage || 0;
          this.groupMessages_has_more =
            (res.data.currentPage || 0) + 1 < (res.data.totalPages || 0);

          if (page === 0) {
            this.groupMessages = items;
            setTimeout(() => this.scroll_to_bottom(), 100);
          } else {
            this.groupMessages = [...items, ...this.groupMessages];
          }
        }
        this.groupMessages_loading = false;
      },
      error: () => (this.groupMessages_loading = false),
    });
  }

  load_more_group_messages(): void {
    if (this.groupMessages_has_more && !this.groupMessages_loading) {
      this.load_group_messages(this.groupMessages_page + 1);
    }
  }

  send_group_message(): void {
    if (!this.activeGroup) return;
    const content = this.new_message_noi_dung.trim();
    if (!content || this.sending) return;

    this.sending = true;
    const dto: GuiTinNhanDTO = {
      phongChatId: this.activeGroup.id,
      noiDung: content,
      loai: LoaiTinNhan.VAN_BAN,
    };

    this.groupChatService.sendMessage(dto).subscribe({
      next: (res) => {
        if (res.data) {
          this.groupMessages.push(res.data);
          setTimeout(() => this.scroll_to_bottom(), 100);
        }
        this.new_message_noi_dung = '';
        this.sending = false;
        this.showEmojiPicker.set(false);
        if (this.chatInput?.nativeElement) {
          this.chatInput.nativeElement.style.height = 'auto';
        }
      },
      error: () => (this.sending = false),
    });
  }

  mark_group_as_read(roomId: number): void {
    this.groupChatService.markAsRead(roomId).subscribe();
    const group = this.groups.find((g) => g.id === roomId);
    if (group) {
      group.soTinNhanChuaDoc = 0;
    }
  }

  // Handle group WebSocket messages
  private handleGroupWsMessage(msg: any): void {
    if (!msg) return;

    if (msg.type === 'NEW_CHAT_MESSAGE' && msg.data) {
      const newMsg = msg.data as TinNhan;
      if (this.activeGroup && newMsg.phongChatId === this.activeGroup.id) {
        // Check if message already exists
        if (!this.groupMessages.find((m) => m.id === newMsg.id)) {
          this.groupMessages.push(newMsg);
          setTimeout(() => this.scroll_to_bottom(), 100);
        }
      }

      // Update group list
      const group = this.groups.find((g) => g.id === newMsg.phongChatId);
      if (group) {
        group.tinNhanCuoi = newMsg.noiDung;
        group.thoiGianTinNhanCuoi = newMsg.guiLuc;
        if (!this.activeGroup || this.activeGroup.id !== group.id) {
          group.soTinNhanChuaDoc = (group.soTinNhanChuaDoc || 0) + 1;
        }
        // Move to top
        const idx = this.groups.indexOf(group);
        if (idx > 0) {
          this.groups.splice(idx, 1);
          this.groups.unshift(group);
        }
      }
    }
  }

  // === CREATE GROUP MODAL ===
  openCreateGroupModal(): void {
    this.showCreateGroupModal = true;
    this.newGroupName = '';
    this.selectedMemberIds = [];
  }

  closeCreateGroupModal(): void {
    this.showCreateGroupModal = false;
  }

  toggleMemberSelection(userId: number): void {
    const idx = this.selectedMemberIds.indexOf(userId);
    if (idx >= 0) {
      this.selectedMemberIds.splice(idx, 1);
    } else {
      this.selectedMemberIds.push(userId);
    }
  }

  isMemberSelected(userId: number): boolean {
    return this.selectedMemberIds.includes(userId);
  }

  createGroup(): void {
    if (!this.newGroupName.trim() || this.selectedMemberIds.length === 0) return;

    this.groupChatService
      .createRoom({
        ten: this.newGroupName.trim(),
        thanhVienIds: this.selectedMemberIds,
      })
      .subscribe({
        next: (res) => {
          if (res.data) {
            this.groups.unshift(res.data);
            this.closeCreateGroupModal();
            this.switchTab('groups');
            this.select_group(res.data);
          }
        },
        error: (err) => {
          console.error('Error creating group:', err);
        },
      });
  }

  // Get display name for group
  getGroupDisplayName(group: PhongChat): string {
    if (group.ten) return group.ten;
    if (group.loai === LoaiPhongChat.DON && group.nguoiChat) {
      return group.nguoiChat.ten;
    }
    return 'Nhóm chat';
  }

  // Get avatar for group
  getGroupAvatar(group: PhongChat): string {
    if (group.anhNhom) {
      return `http://localhost:8088/api/v1/users/profile-images/${group.anhNhom}`;
    }
    if (group.loai === LoaiPhongChat.DON && group.nguoiChat?.anhDaiDien) {
      return `http://localhost:8088/api/v1/users/profile-images/${group.nguoiChat.anhDaiDien}`;
    }
    return `https://ui-avatars.com/api/?name=${encodeURIComponent(
      this.getGroupDisplayName(group)
    )}&background=random&color=fff&size=128`;
  }

  // Universal send message (friend or group)
  sendMessageUniversal(): void {
    if (this.activeChatType === 'friend') {
      this.send_message();
    } else if (this.activeChatType === 'group') {
      this.send_group_message();
    }
  }

  // === GROUP SETTINGS ===
  openGroupSettings(): void {
    if (!this.activeGroup) return;
    this.showGroupSettings = true;
    this.editGroupName = this.activeGroup.ten || '';
    this.isEditingGroupName = false;
  }

  closeGroupSettings(): void {
    this.showGroupSettings = false;
    this.isEditingGroupName = false;
  }

  startEditGroupName(): void {
    this.isEditingGroupName = true;
    this.editGroupName = this.activeGroup?.ten || '';
  }

  cancelEditGroupName(): void {
    this.isEditingGroupName = false;
  }

  saveGroupName(): void {
    if (!this.activeGroup || !this.editGroupName.trim()) return;

    this.groupChatService
      .updateRoom(this.activeGroup.id, {
        ten: this.editGroupName.trim(),
      })
      .subscribe({
        next: (res) => {
          if (res.data && this.activeGroup) {
            this.activeGroup.ten = res.data.ten;
            // Update in groups list
            const idx = this.groups.findIndex((g) => g.id === this.activeGroup!.id);
            if (idx >= 0) {
              this.groups[idx].ten = res.data.ten;
            }
            this.isEditingGroupName = false;
            this.toastr.success('Đã đổi tên nhóm');
          }
        },
        error: () => this.toastr.error('Không thể đổi tên nhóm'),
      });
  }

  // === GROUP AVATAR ===
  onGroupAvatarSelected(event: Event): void {
    const input = event.target as HTMLInputElement;
    if (!input.files?.length || !this.activeGroup) return;

    const file = input.files[0];
    if (!file.type.startsWith('image/')) {
      this.toastr.error('Vui lòng chọn file ảnh');
      return;
    }

    // Upload avatar
    const formData = new FormData();
    formData.append('file', file);

    this.http
      .post<ResponseObject<string>>(`${this.API_URL}/users/upload-avatar`, formData, {
        headers: this.httpUtil.createUploadHeaders(),
      })
      .subscribe({
        next: (res) => {
          if (res.data && this.activeGroup) {
            // Update group with new avatar
            this.groupChatService
              .updateRoom(this.activeGroup.id, {
                anhNhom: res.data,
              })
              .subscribe({
                next: (updateRes) => {
                  if (updateRes.data && this.activeGroup) {
                    this.activeGroup.anhNhom = updateRes.data.anhNhom;
                    const idx = this.groups.findIndex((g) => g.id === this.activeGroup!.id);
                    if (idx >= 0) {
                      this.groups[idx].anhNhom = updateRes.data.anhNhom;
                    }
                    this.toastr.success('Đã cập nhật ảnh nhóm');
                  }
                },
              });
          }
        },
        error: () => this.toastr.error('Không thể tải lên ảnh'),
      });
  }

  // === ADD MEMBERS ===
  openAddMembersModal(): void {
    this.showAddMembersModal = true;
    this.addMemberSelectedIds = [];
  }

  closeAddMembersModal(): void {
    this.showAddMembersModal = false;
  }

  toggleAddMemberSelection(userId: number): void {
    const idx = this.addMemberSelectedIds.indexOf(userId);
    if (idx >= 0) {
      this.addMemberSelectedIds.splice(idx, 1);
    } else {
      this.addMemberSelectedIds.push(userId);
    }
  }

  isAddMemberSelected(userId: number): boolean {
    return this.addMemberSelectedIds.includes(userId);
  }

  isAlreadyMember(userId: number): boolean {
    return this.activeGroup?.thanhVien?.some((m) => m.nguoiDungId === userId) || false;
  }

  addMembersToGroup(): void {
    if (!this.activeGroup || this.addMemberSelectedIds.length === 0) return;

    this.groupChatService
      .updateRoom(this.activeGroup.id, {
        themThanhVien: this.addMemberSelectedIds,
      })
      .subscribe({
        next: (res) => {
          if (res.data && this.activeGroup) {
            this.activeGroup.thanhVien = res.data.thanhVien;
            const idx = this.groups.findIndex((g) => g.id === this.activeGroup!.id);
            if (idx >= 0) {
              this.groups[idx].thanhVien = res.data.thanhVien;
            }
            this.closeAddMembersModal();
            this.toastr.success(`Đã thêm ${this.addMemberSelectedIds.length} thành viên`);
          }
        },
        error: () => this.toastr.error('Không thể thêm thành viên'),
      });
  }

  // === KICK MEMBER ===
  kickMember(memberId: number, memberName: string): void {
    if (!this.activeGroup) return;

    if (!confirm(`Bạn có chắc muốn xóa ${memberName} khỏi nhóm?`)) return;

    this.groupChatService
      .updateRoom(this.activeGroup.id, {
        xoaThanhVien: [memberId],
      })
      .subscribe({
        next: (res) => {
          if (res.data && this.activeGroup) {
            this.activeGroup.thanhVien = res.data.thanhVien;
            const idx = this.groups.findIndex((g) => g.id === this.activeGroup!.id);
            if (idx >= 0) {
              this.groups[idx].thanhVien = res.data.thanhVien;
            }
            this.toastr.success(`Đã xóa ${memberName} khỏi nhóm`);
          }
        },
        error: () => this.toastr.error('Không thể xóa thành viên'),
      });
  }

  // === LEAVE GROUP ===
  leaveGroup(): void {
    if (!this.activeGroup) return;

    if (!confirm('Bạn có chắc muốn rời khỏi nhóm này?')) return;

    this.groupChatService.leaveRoom(this.activeGroup.id).subscribe({
      next: () => {
        const idx = this.groups.findIndex((g) => g.id === this.activeGroup!.id);
        if (idx >= 0) {
          this.groups.splice(idx, 1);
        }
        this.activeGroup = null;
        this.activeChatType = null;
        this.closeGroupSettings();
        this.toastr.success('Đã rời khỏi nhóm');
      },
      error: () => this.toastr.error('Không thể rời khỏi nhóm'),
    });
  }

  // === FILE/MEDIA MESSAGE ===
  openFileSelector(): void {
    this.fileInput?.nativeElement.click();
  }

  onFileSelected(event: Event): void {
    const input = event.target as HTMLInputElement;
    if (!input.files?.length) return;

    this.selectedFile = input.files[0];
    this.uploadAndSendFile();
  }

  uploadAndSendFile(): void {
    if (!this.selectedFile || this.uploadingFile) return;

    // Check if there's an active chat
    if (!this.activeChatType) {
      this.toastr.warning('Vui lòng chọn cuộc trò chuyện');
      return;
    }

    this.uploadingFile = true;
    const file = this.selectedFile;
    const formData = new FormData();
    formData.append('file', file);

    // Determine file type
    let loaiGroup: LoaiTinNhan = LoaiTinNhan.TAP_TIN;
    let loaiFriend: 'VAN_BAN' | 'HINH_ANH' | 'TAP_TIN' | 'AM_THANH' = 'TAP_TIN';

    if (file.type.startsWith('image/')) {
      loaiGroup = LoaiTinNhan.HINH_ANH;
      loaiFriend = 'HINH_ANH';
    } else if (file.type.startsWith('audio/')) {
      loaiGroup = LoaiTinNhan.AM_THANH;
      loaiFriend = 'AM_THANH';
    }

    // Upload file first - use the new chat upload endpoint
    // Note: Use createUploadHeaders() (no Content-Type) for FormData upload
    this.http
      .post<ResponseObject<string>>(`${this.API_URL}/chat/upload`, formData, {
        headers: this.httpUtil.createUploadHeaders(),
      })
      .subscribe({
        next: (res) => {
          if (res.data) {
            const fileUrl = res.data;

            if (this.activeChatType === 'group' && this.activeGroup) {
              // Send to group chat
              const dto: GuiTinNhanDTO = {
                phongChatId: this.activeGroup.id,
                loai: loaiGroup,
                urlMedia: fileUrl,
                tenFile: file.name,
                kichThuocFile: file.size,
              };

              this.groupChatService.sendMessage(dto).subscribe({
                next: (msgRes) => {
                  if (msgRes.data) {
                    this.groupMessages.push(msgRes.data);
                    setTimeout(() => this.scroll_to_bottom(), 100);
                  }
                  this.finishUpload();
                },
                error: () => {
                  this.finishUpload();
                  this.toastr.error('Không thể gửi file');
                },
              });
            } else if (this.activeChatType === 'friend' && this.active_partner) {
              // Send to friend chat (1-1)
              const dto = new SendMessageDto({
                receiver_id: this.active_partner.partner_id,
                loai_tin_nhan: loaiFriend,
                url_media: fileUrl,
                ten_file: file.name,
                kich_thuoc_file: file.size,
              });

              this.chatService.sendMessage(dto).subscribe({
                next: (msgRes) => {
                  if (msgRes.data) {
                    this.messages.push(msgRes.data);
                    setTimeout(() => this.scroll_to_bottom(), 100);
                  }
                  this.finishUpload();
                },
                error: () => {
                  this.finishUpload();
                  this.toastr.error('Không thể gửi file');
                },
              });
            }
          }
        },
        error: () => {
          this.finishUpload();
          this.toastr.error('Không thể tải lên file');
        },
      });
  }

  private finishUpload(): void {
    this.uploadingFile = false;
    this.selectedFile = null;
    if (this.fileInput) {
      this.fileInput.nativeElement.value = '';
    }
  }

  // === SEND IMAGE MESSAGE ===
  openImageSelector(): void {
    const input = document.createElement('input');
    input.type = 'file';
    input.accept = 'image/*';
    input.onchange = (e: any) => {
      const file = e.target.files[0];
      if (file) {
        this.selectedFile = file;
        this.uploadAndSendFile();
      }
    };
    input.click();
  }

  // Check if current user is admin of group
  isGroupAdmin(): boolean {
    if (!this.activeGroup || !this.current_user_id) return false;
    const member = this.activeGroup.thanhVien?.find((m) => m.nguoiDungId === this.current_user_id);
    return member?.vaiTro === 'ADMIN';
  }

  // Format file size
  formatFileSize(bytes?: number): string {
    if (!bytes) return '0 B';
    const k = 1024;
    const sizes = ['B', 'KB', 'MB', 'GB'];
    const i = Math.floor(Math.log(bytes) / Math.log(k));
    return parseFloat((bytes / Math.pow(k, i)).toFixed(1)) + ' ' + sizes[i];
  }
}
