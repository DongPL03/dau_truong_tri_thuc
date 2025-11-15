import {Component, computed, effect, OnDestroy, OnInit, signal} from '@angular/core';
import {CommonModule} from '@angular/common';
import {FormsModule} from '@angular/forms';
import {Base} from '../../base/base';
import {TranDauResponse} from '../../../responses/trandau/trandau-response';
import {SyncStateResponse} from '../../../responses/trandau/syncstate-response';
import {ResponseObject} from '../../../responses/response-object';
import Swal from 'sweetalert2';
import {ThamGiaTranDauDTO} from '../../../dtos/tran-dau/thamgiatrandau-dto';
import {RoiTranDauDTO} from '../../../dtos/tran-dau/roitran-dto';
import {SubmitAnswerDTO} from '../../../dtos/tran-dau/submitanswer-dto';
import {BattleEvent} from '../../../services/ws-trandau.service';
import {environment} from '../../../environments/environment';
import {FinishedPlayer} from '../../../responses/trandau/finished-player';

@Component({
  selector: 'app-chi-tiet-phong',
  imports: [CommonModule, FormsModule],
  templateUrl: './chi-tiet-phong.html',
  styleUrl: './chi-tiet-phong.scss',
  standalone: true
})
export class ChiTietPhong extends Base implements OnInit, OnDestroy {
  showSummary = signal<boolean>(false);
  finalResult?: {
    winner: any;
    leaderboard: any[];
    myId: number;
  };

  preCountdown = signal<number>(0);
  private preCountdownTimer?: ReturnType<typeof setInterval>;

  loading = signal<boolean>(true);
  saving = signal<boolean>(false);


  battle = signal<TranDauResponse | null>(null);
  syncState = signal<SyncStateResponse | null>(null);

  leaderboard = signal<LeaderboardPlayer[]>([]);

  isHostUservariable = false;

  protected readonly environment = environment;
  protected readonly console = console;


  pageTitle = computed(() => this.battle()?.ten_phong ?? 'Phòng');
  status = computed(() => this.battle()?.trang_thai ?? 'PENDING');
  isPending = computed(() => this.status() === 'PENDING');
  isOngoing = computed(() => this.status() === 'ONGOING');
  isFinished = computed(() => this.status() === 'FINISHED');

  // join/submit local fields
  pinCode = signal<string>('');
  selectedAnswer = signal<'A' | 'B' | 'C' | 'D' | ''>('');

// simple ticking countdown (client-side) derived from sync
  remainingSeconds = signal<number>(0);
  private timer?: ReturnType<typeof setInterval>;

  // ====== SUMMARY STATE ======
  mySummaryRow?: FinishedPlayer;
  isWinnerMe = false;

// ====== CHỐNG NỘP NHIỀU LẦN ======
  submittedCurrentAnswer = signal<boolean>(false);


  constructor() {
    super();
    effect(() => {
      const s = this.syncState();
      if (!s || s.current_question_index < 0) {
        this.clearTimer();
        this.remainingSeconds.set(0);
        return;
      }
      // tránh gọi tick nếu chưa có thời gian bắt đầu
      const startIso = s.current_question_start ? Date.parse(s.current_question_start) : NaN;
      if (isNaN(startIso)) {
        console.warn('⏸️ Bỏ qua effect tick() vì current_question_start chưa hợp lệ:', s.current_question_start);
        return;
      }
      const endAt = startIso + s.seconds_per_question * 1000;
      // nếu timer đã chạy rồi, không khởi động lại nữa
      if (this.timer) {
        return;
      }
      console.log('💡 Effect tick() được kích hoạt cho câu', s.current_question_index + 1);
      this.tick(endAt);
    });


    effect(() => {
      const s = this.syncState();
      if (s && s.current_question_index >= 0) {
        this.selectedAnswer.set('');
      }
    });
  }

  ngOnInit(): void {
    const id = Number(this.route.snapshot.paramMap.get('id'));
    if (!id) return;

    // ✅ Lấy token + user
    const token = this.tokenService.getAccessToken();
    const user = this.userService.currentUser();

    // ✅ Kết nối WS duy nhất và lắng nghe realtime
    // @ts-ignore
    this.wsTrandauService.connect(() => token, user.id, id)
      .then(() => {
        console.log('✅ WebSocket connected to backend!');
        this.wsTrandauService.subscribeBattle(id, (ev) => this.handleBattleEvent(ev));
      })
      .catch(err => console.error('❌ WebSocket connect failed:', err));

    // ✅ Lấy thông tin phòng & đồng bộ ban đầu
    this.fetchDetail(id, () => this.doSync());
    this.currentUserName();
    setTimeout(() => this.syncState.update(s => s ? {...s} : s), 200);
  }

  ngOnDestroy() {
    this.clearTimer();
    this.wsTrandauService.disconnect();
  }


// =====================================================
// REST API
// =====================================================


  fetchDetail(id: number, next ?: () => void) {
    this.loading.set(true);
    this.tranDauService.getBattleDetail(id).subscribe({
      next: (res: ResponseObject<TranDauResponse>) => {
        this.battle.set(res.data!);
        this.loading.set(false);
        next?.();
      },
      error: () => {
        this.loading.set(false);
        Swal.fire('Lỗi', 'Không thể tải thông tin phòng', 'error').then(() => this.router.navigateByUrl('/home'));
      },
    });
  }

  doSync() {
    const id = this.battle()?.id;
    if (!id) return;
    this.tranDauService.sync(id).subscribe({
      next: (res: ResponseObject<SyncStateResponse>) => {
        this.syncState.set(res.data!);

        // ✅ ép effect chạy lại nếu có câu hỏi đầu tiên
        const s = res.data!;
      }
    });
  }

  isHostUser(): boolean {
    const b = this.battle();
    const u = this.userService.currentUser();
    if (!b || !u) return false;
    if (b.chu_phong_ten === u.ho_ten) return true;
    return false;
  }

// =====================================================
// THAM GIA PHÒNG + KẾT NỐI WS
// =====================================================
  join() {
    const b = this.battle();
    if (!b) return;
    const dto: ThamGiaTranDauDTO = {tran_dau_id: b.id};
    if (!b.cong_khai) dto.ma_pin = this.pinCode();
    this.saving.set(true);

    this.tranDauService.joinBattle(dto as any).subscribe({
      next: () => {
        this.saving.set(false);
        Swal.fire('Thành công', 'Bạn đã tham gia phòng', 'success').then(r => {
        });
        this.doSync();
      },
      error: (e) => {
        this.saving.set(false);
        Swal.fire('Không thể tham gia', e?.error?.message || 'Vui lòng kiểm tra lại', 'error').then(r => {
        });
      },
    });
  }

  leave() {
    const b = this.battle();
    if (!b) return;
    Swal.fire({title: 'Rời phòng?', icon: 'question', showCancelButton: true}).then((r) => {
      if (!r.isConfirmed) return;
      const dto: RoiTranDauDTO = {tran_dau_id: b.id};
      this.saving.set(true);
      this.tranDauService.leaveBattle(dto as any).subscribe({
        next: () => {
          this.saving.set(false);
          Swal.fire('Đã rời phòng', '', 'success').then(r => {
          });
          this.wsTrandauService.disconnect();
          this.router.navigateByUrl('/home').then(r => {
          });
        },
        error: (e) => {
          this.saving.set(false);
          Swal.fire('Không thể rời phòng', e?.error?.message || 'Thử lại sau', 'error').then(r => {
          });
        },
      });
    });
  }

// =====================================================
// WEBSOCKET EVENT HANDLER
// =====================================================
  handleBattleEvent(evt: BattleEvent) {
    console.log('📡 WS Event:', evt);
    switch (evt.type) {
      case 'PLAYER_JOINED':
        Swal.fire('👋 Người chơi mới', `${evt.ho_ten} vừa tham gia phòng`, 'info').then(r => {
        });
        break;
      case 'PLAYER_LEFT':
        Swal.fire('🚪 Người chơi rời đi', `${evt.ho_ten} đã rời phòng`, 'warning').then(r => {
        });
        break;
      case 'BATTLE_STARTED':
        Swal.fire({
          icon: 'success',
          title: 'Trận đấu bắt đầu!',
          text: `Phòng: ${evt.ten_phong} (${evt.tong_cau_hoi} câu hỏi, ${evt.thoi_gian_moi_cau_giay}s mỗi câu)`,
          timer: 1800,
          showConfirmButton: false
        }).then(r => {
        });

        this.battle.update((b) => ({
          ...b!,
          trang_thai: 'ONGOING',
          bat_dau_luc: evt.bat_dau_luc
        }));

        const pre = (evt as any).dem_nguoc_truoc_giay ?? 0;
        if (pre > 0) {
          this.startPreCountdown(pre);
        }

        // ✅ Gọi sync 2 lần an toàn để chắc chắn nhận câu đầu tiên
        const doInitSync = () => {
          this.tranDauService.sync(evt.tran_dau_id).subscribe({
            next: (res: ResponseObject<SyncStateResponse>) => {
              const s = res.data!;
              this.syncState.set(s);
            }
          });
        };
        setTimeout(doInitSync, 200); // lần đầu sync
        setTimeout(doInitSync, 700); // lần hai backup nếu backend gửi trễ
        break;

      case 'NEW_QUESTION': {
        const q = evt.question;
        if (!q) return;

        const newState = {
          tran_dau_id: evt.tran_dau_id,
          current_question_index: evt.question_index,
          current_question_id: q.id,
          seconds_per_question: evt.thoi_gian_cau_giay,
          // 💡 dùng timestamp từ server:
          current_question_start: evt.timestamp,
          noi_dung: q.noi_dung,
          loai_noi_dung: q.loai_noi_dung,
          duong_dan_tep: q.duong_dan_tep,
          a: q.lua_chon_a,
          b: q.lua_chon_b,
          c: q.lua_chon_c,
          d: q.lua_chon_d,
          my_total_points: this.syncState()?.my_total_points ?? 0,
          _version: Math.random(),
        };

        this.syncState.set({...newState});

        // ⛔ reset trạng thái nộp của câu mới
        this.submittedCurrentAnswer.set(false);
        this.selectedAnswer.set('');

        const startMs = Date.parse(newState.current_question_start);
        if (isNaN(startMs)) {
          console.warn('⚠️ current_question_start không hợp lệ:', newState.current_question_start);
          return;
        }
        this.clearTimer();
        this.tick(startMs + newState.seconds_per_question * 1000);
        break;
      }

      case 'SCORE_UPDATE':
        const myId = this.userService.getUserId();
        if (evt.user_id !== myId) {
          // Người khác được cập nhật điểm => chỉ cập nhật leaderboard, không popup
          console.log(`📡 SCORE_UPDATE từ người khác (${evt.ho_ten}), bỏ qua popup.`);
          // this.toastService.show(`${evt.ho_ten} đã nộp đáp án`, {type: 'info'});
          return;
        }

        Swal.fire(
          evt.correct ? '✅ Chính xác!' : '❌ Sai mất rồi',
          `+${evt.gained_points} điểm`,
          evt.correct ? 'success' : 'error'
        ).then(() => {
        });

        setTimeout(() => {
          this.syncState.update(s => s ? {...s, my_total_points: evt.total_points} : s);
        }, 300);
        break;


      case 'LEADERBOARD_UPDATE':
        // 🏅 Cập nhật leaderboard
        // @ts-ignore
        this.leaderboard.set(evt.players || []);
        break;


      case 'FINISHED': {
        console.log('🏁 Trận đấu kết thúc', evt);
        this.battle.set({...(this.battle() as TranDauResponse), trang_thai: 'FINISHED'});

        const myId = this.userService.getUserId();

        // 🧮 Lưu kết quả tạm để hiển thị ở màn hình summary
        this.finalResult = {
          winner: evt.winner,
          leaderboard: evt.leaderboard as FinishedPlayer[],
          myId,
        };

        // tìm dòng của chính mình
        this.mySummaryRow = this.finalResult.leaderboard.find(p => p.user_id === myId);
        this.isWinnerMe = !!(this.finalResult.winner && this.finalResult.winner.user_id === myId);


        // 🧭 Chuyển trạng thái sang summary view
        this.showSummary.set(true);
        this.clearTimer();
        break;
      }
    }
  }

  startBattle() {
    const id = this.battle()?.id;
    if (!id) return;
    this.saving.set(true);
    this.tranDauService.startBattle(id).subscribe({
      next: (res) => {
        this.saving.set(false);
        this.battle.set({...(this.battle() as TranDauResponse), trang_thai: 'ONGOING'});
        Swal.fire('Bắt đầu!', 'Trận đấu đã bắt đầu', 'success').then(r => {
        });
        this.doSync();
      },
      error: (e) => {
        this.saving.set(false);
        Swal.fire('Không thể bắt đầu', e?.error?.message || 'Bạn có quyền chủ phòng?', 'error').then(r => {
        });
      }
    });
  }


  finishBattle() {
    const id = this.battle()?.id;
    if (!id) return;
    Swal.fire({
      title: 'Kết thúc trận?',
      text: 'Hệ thống sẽ chốt điểm & phát kết quả',
      icon: 'warning',
      showCancelButton: true
    }).then(r => {
      if (!r.isConfirmed) return;
      this.saving.set(true);
      this.tranDauService.finishBattle(id).subscribe({
        next: () => {
          this.saving.set(false);
          this.battle.set({...(this.battle() as TranDauResponse), trang_thai: 'FINISHED'});
          Swal.fire('Đã kết thúc', 'Xem bảng xếp hạng ở phía dưới', 'success').then(r => {
          });
          this.doSync();
        },
        error: (e) => {
          this.saving.set(false);
          Swal.fire('Không thể kết thúc', e?.error?.message || 'Thử lại sau', 'error').then(r => {
          });
        }
      });
    });
  }

  submitSelectedAnswer() {
    const s = this.syncState();
    const b = this.battle();
    if (!s || !b || s.current_question_index < 0 || !s.current_question_id) return;

    // ⛔ đã nộp câu hiện tại rồi
    if (this.submittedCurrentAnswer()) {
      Swal.fire('Bạn đã nộp đáp án', 'Hãy chờ câu hỏi tiếp theo nhé', 'info').then(() => {
      });
      return;
    }

    const ans = this.selectedAnswer();
    if (!ans) {
      Swal.fire('Chưa chọn đáp án', 'Hãy chọn A/B/C/D', 'info').then(r => {
      });
      return;
    }

    const dto: SubmitAnswerDTO = {tran_dau_id: b.id, cau_hoi_id: s.current_question_id, answer: ans};
    // ✅ Đánh dấu đã nộp NGAY LẬP TỨC để chặn double-click
    this.submittedCurrentAnswer.set(true);
    this.saving.set(true);
    this.tranDauService.submitAnswer(dto as any).subscribe({
      next: (res) => {
        this.saving.set(false);
      },
      error: (e) => {
        this.saving.set(false);
        // Nếu lỗi thì cho phép nộp lại
        this.submittedCurrentAnswer.set(false);
        Swal.fire('Không thể nộp đáp án', e?.error?.message || 'Thử lại sau', 'error').then(r => {
        });
      }
    });
  }


// =====================================================
// TIMER
// =====================================================

  tick(endAtMs: number) {
    if (!endAtMs || isNaN(endAtMs)) {
      console.warn('⏱️ Bỏ qua tick() vì endAtMs không hợp lệ:', endAtMs);
      return;
    }
    this.clearTimer();
    const run = () => {
      const remain = Math.max(0, Math.floor((endAtMs - Date.now()) / 1000));
      this.remainingSeconds.set(remain);
      if (remain <= 0) this.clearTimer();
    };

    run();
    this.timer = setInterval(run, 1000);
  }


  clearTimer() {
    if (this.timer) {
      clearInterval(this.timer);
      this.timer = undefined;
    }
  }

  badgeClass(status: string | undefined) {
    switch (status) {
      case 'PENDING':
        return 'badge pending';
      case 'ONGOING':
        return 'badge ongoing';
      case 'FINISHED':
        return 'badge finished';
      default:
        return 'badge';
    }
  }

  selectAnswer(opt: string) {
    this.selectedAnswer.set(opt as any);
  }


  goBackToBattleList() {
    this.router.navigateByUrl('/battle').then(r => {
    }); // hoặc '/battle/list'
  }

  goBackHome() {
    this.router.navigateByUrl('/home').then(r => {
    });
  }

  get summary_leaderboard(): FinishedPlayer[] {
    if (!this.finalResult?.leaderboard) {
      return [];
    }
    // đảm bảo sort theo xếp_hang
    return [...this.finalResult.leaderboard].sort((a, b) => a.xep_hang - b.xep_hang);
  }

  get summary_winner(): FinishedPlayer | null {
    // kiểu ở Winner trong WS gần như giống FinishedPlayer, mình ép về cho đồng nhất
    const w = this.finalResult?.winner as FinishedPlayer | undefined;
    return w ?? null;
  }

  is_my_row(p: FinishedPlayer): boolean {
    return this.finalResult?.myId === p.user_id;
  }

  is_winner_row(p: FinishedPlayer): boolean {
    return this.finalResult?.winner?.user_id === p.user_id;
  }

  practiceThisSet() {
    const b = this.battle();
    if (!b) return;
    // ví dụ: mở trang chi tiết bộ câu hỏi để luyện tập lại
    this.router.navigate(['/bo-cau-hoi/detail', b.bo_cau_hoi_id]).then(r => {
    });
  }


  startPreCountdown(seconds: number) {
    if (this.preCountdownTimer) {
      clearInterval(this.preCountdownTimer);
    }
    this.preCountdown.set(seconds);

    this.preCountdownTimer = setInterval(() => {
      const cur = this.preCountdown();
      if (cur <= 1) {
        this.preCountdown.set(0);
        clearInterval(this.preCountdownTimer!);
        this.preCountdownTimer = undefined;
      } else {
        this.preCountdown.set(cur - 1);
      }
    }, 1000);
  }


  currentUserName() {
    const u = this.userService.currentUser();
    return u ? u.ho_ten : 'Người chơi';
  }
}


